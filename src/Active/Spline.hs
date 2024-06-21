{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- IMPORTANT TODO CABAL REPL is the command to use for libraries

module Active.Spline
  ( Tweenable,
    Spline,
    runSpline,
    createLinearSpline,
    createBezierSpline,
    createHermiteSpline,
    createCatmullRomSpline,
    createBSpline,
  )
where

import Data.Char (chr, ord)
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Debug.Trace (trace)

-- Tweenable class definition
class Tweenable a where
  lerp :: a -> a -> Rational -> a

-- Some basic members
newtype FractionalTweenable a = FractionalTweenable a

instance (Fractional a) => Tweenable (FractionalTweenable a) where
  lerp (FractionalTweenable v1) (FractionalTweenable v2) val =
    FractionalTweenable $ v1 + fromRational val * (v2 - v1)

-- DerivingVia
deriving via FractionalTweenable Float instance Tweenable Float

deriving via FractionalTweenable Double instance Tweenable Double

deriving via FractionalTweenable Rational instance Tweenable Rational

-- Other simple examples
instance (Tweenable a) => Tweenable (Maybe a) where
  lerp Nothing _ _ = Nothing
  lerp _ Nothing _ = Nothing
  lerp (Just a1) (Just a2) val = Just (lerp a1 a2 val)

instance (Tweenable a, Tweenable b) => Tweenable (a, b) where
  lerp (a1, b1) (a2, b2) val = (lerp a1 a2 val, lerp b1 b2 val)

instance (Tweenable a, Tweenable b, Tweenable c) => Tweenable (a, b, c) where
  lerp (a1, b1, c1) (a2, b2, c2) val = (lerp a1 a2 val, lerp b1 b2 val, lerp c1 c2 val)

-- Tweenable class
class (Tweenable (TweenType a)) => TweenableT a where
  type TweenType a -- Associated type with type constraint
  forward :: a -> TweenType a
  backward :: TweenType a -> a

instance {-# OVERLAPPABLE #-} (TweenableT a) => Tweenable a where -- pragma
  lerp :: a -> a -> Rational -> a
  lerp v1 v2 val = backward $ lerp (forward v1) (forward v2) val

newtype IntegralTweenableT a = IntegralTweenableT a

instance (Integral a) => TweenableT (IntegralTweenableT a) where
  type TweenType (IntegralTweenableT a) = Float
  forward (IntegralTweenableT a) = fromIntegral a
  backward f = IntegralTweenableT $ round f

deriving via IntegralTweenableT Int instance TweenableT Int

deriving via IntegralTweenableT Integer instance TweenableT Integer

instance TweenableT Char where
  type TweenType Char = Int
  forward = ord
  backward = chr

-- TODO rename forward and backWard to "toTweenable" and "fromTweenable"

-- data Active a where
--   Prim :: Rational -> (Rational -> a) -> Active a
--   SplinePrim :: (Spline s a) => Rational -> s a -> Active a

-- FINALLY, SOME SPLINES
class Spline s a where
  type ControlType s a
  runSpline :: s a -> Rational -> a

-- getControlPoints :: s a -> [ControlType s a]

-- changeControlPoint :: s a -> Int -> a -> s a   TODO
-- getDuration :: s c a -> Rational    TODO

-- Linear spline
newtype LinearSpline a = LinearSpline [a]

createLinearSpline :: [a] -> LinearSpline a
createLinearSpline [] = error "TODO"
createLinearSpline as = LinearSpline as

instance (Tweenable a) => Spline LinearSpline a where
  runSpline :: LinearSpline a -> Rational -> a
  runSpline (LinearSpline as) t =
    lerp v1 v2 inter
    where
      inter = t - fromIntegral index
      index = floor t
      v1 = as !! index
      v2 = as !! (index + 1)

  type ControlType LinearSpline a = a

-- getControlPoints (LinearSpline as) = as

-- Bezier spline
newtype BezierSpline a = BezierSpline [a]

createBezierSpline :: [a] -> BezierSpline a
createBezierSpline as
  | length as - 1 `mod` 3 /= 0 = error "TODO"
  | otherwise = BezierSpline as

instance (Tweenable a) => Spline BezierSpline a where
  runSpline :: BezierSpline a -> Rational -> a
  runSpline (BezierSpline as) t = result
    where
      as' = [as !! (floor t * 3 + i) | i <- [0 .. 3]]

      interFunc a1 a2 = lerp a1 a2 (t - fromIntegral (floor t))
      collapseFunc as = zipWith interFunc as (tail as)
      [result] = iterate collapseFunc as' !! 3
  type ControlType BezierSpline a = a

-- getControlPoints (BezierSpline as) = as

-- Spline from a characteristic matrix
class SplineCharMatrix s a where -- TODO bad idea
  charMatrix :: s a -> M.Matrix a
  parameters :: s a -> Int -> [a]

instance (SplineCharMatrix s a, Floating a) => Spline s a where
  runSpline spline t = M.getElem 1 1 result
    where
      t' = t - fromIntegral (floor t)

      baseMat = M.fromList 1 4 $ map fromRational [1, t', t' * t', t' * t' * t']
      characteristicMat = charMatrix spline
      points = M.fromList 4 1 $ parameters spline (floor t)

      result = baseMat `M.multStd` characteristicMat `M.multStd` points

-- Hermite spline
newtype HermiteSpline a = HermiteSpline [(a, a)] -- TODO write a constructor

createHermiteSpline :: [(a, a)] -> HermiteSpline a
createHermiteSpline = HermiteSpline

instance (Floating a) => SplineCharMatrix HermiteSpline a where
  charMatrix (HermiteSpline as) =
    M.fromLists
      [ [1, 0, 0, 0],
        [0, 1, 0, 0],
        [-3, -2, 3, -1],
        [2, 1, -2, 1]
      ]
  parameters (HermiteSpline as) index = [p0, v0, p1, v1]
    where
      (p0, v0) = as !! min (length as - 1) index
      (p1, v1) = as !! min (length as - 1) (index + 1)

-- Catmull-rom spline
newtype CatmullRomSpline a = CatmullRomSpline [a]

createCatmullRomSpline :: (Floating a) => [a] -> CatmullRomSpline a
createCatmullRomSpline ps
  | length ps < 2 = error "TODO A spline needs at least two points to be defined"
  | otherwise = CatmullRomSpline $ [start] <> ps <> [end]
  where
    [start1, start2] = take 2 ps
    [end1, end2] = drop (length ps - 2) ps
    start = start1 + (start1 - start2)
    end = end2 + (end2 - end1)

instance (Floating a) => SplineCharMatrix CatmullRomSpline a where
  charMatrix (CatmullRomSpline as) =
    M.fromLists $
      (map . map)
        (/ 2)
        [ [0, 2, 0, 0],
          [-1, 0, 1, 0],
          [2, -5, 4, -1],
          [-1, 3, -3, 1]
        ]
  parameters (CatmullRomSpline as) index =
    [as !! min (length as - 1) (index + 1 + i) | i <- [0 .. 3]]

-- B spline
newtype BSpline a = BSpline [a]

createBSpline :: [a] -> BSpline a
createBSpline = BSpline

instance (Floating a) => SplineCharMatrix BSpline a where
  charMatrix (BSpline as) =
    M.fromLists $
      (map . map)
        (/ 6)
        [ [1, 4, 1, 0],
          [-3, 0, 3, 0],
          [3, -6, 3, 0],
          [-1, 3, -3, 1]
        ]
  parameters (BSpline as) index =
    [as !! min (length as - 1) (index + i) | i <- [0 .. 3]]

-- Testing
linearSpline = createLinearSpline [(1, Just 'a', 1), (2, Just '1', 5)]

-- bezierSpline :: BezierSpline Rational
-- bezierSpline = BezierSpline [1, 2, 3, 4, 5, 6, 7]

-- hermiteSpline :: HermiteSpline Double
-- hermiteSpline = HermiteSpline [(5, -1), (6, -1)]

-- catmullRomSpline :: CatmullRomSpline Double
-- catmullRomSpline = createCatmullRomSpline [5, 6]

-- bSpline :: BSpline Double
-- bSpline = BSpline [5, 6, 7, 8, 9, 10]

-- THINGS LEFT TO IMPLEMENT

-- composing same type splines with equality sequentially
-- composing splines parallel

-- functions to stretch time.

-- CURVES TO REPRESENT

-- linear:      C0      just interpolate
-- bezier:      C0/C1   2 points between
-- hermite:     C0/C1   velocities
-- catmull-rom: C1      special endpoint handling, neigbouring point velocity, scale 0.5
-- b-spline:    C2      matrix representation

-- active questions:

-- maybe instance:
--      should Nothing result in Nothing?
--      should Nothing result in the other thing?
--      should Nothing result in half Nothing, half Other?