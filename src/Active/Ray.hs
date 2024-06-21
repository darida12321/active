-- XXX todo: clean up, make module header etc.
{-# LANGUAGE RankNTypes #-}

module Active.Ray where

import Active.Duration
import Data.Ratio

-- | @Ray c d k p@ represents an arithmetic progression of points in
--   time (i.e. regular samples), contained in a closed interval
--   beginning at @c@ with duration @d@.  @p@ is the "phase shift", so
--   that the first sample is at @c + p@.  In general, the samples are
--   at @c + p + kt@ for natural numbers @t@.
--
--   More abstractly, a @Ray@ represents an affine transformation of
--   some initial segment of \([0,\infty)\) plus a phase shift @p@.
--
--   Invariants: \(0 \leq |p| < |k|\); k and p have the same sign.

-- New rays: start, step, duration
data Ray d = Ray (TimeType d) (TimeType d) d

rayPoints :: (Duration d) => Ray d -> [TimeType d]
rayPoints (Ray start step dur) = takeWhile (dur `containsTime`) $ iterate (`addTime` step) start

primRay :: forall d. (Duration d, LinearDuration d) => d -> Ray d
primRay = Ray nullTime unitTime

cutRay :: (LinearDuration d) => d -> Ray d -> Ray d
cutRay dur' (Ray start step dur)
  | dur `smallerThanDur` dur' = Ray start step dur
  | dur' `smallerThanDur` dur = Ray start step dur'
  | otherwise = error "TODO normal error msg"

rmod :: Rational -> Rational -> Rational
rmod r m = r - m * fromIntegral (floor (r / m))

-- Drop an initial segment of length x from a ray.
-- Assumption: x <= duration of the ray.
omitRay :: (Duration d) => TimeType d -> Ray d -> Ray d
omitRay time (Ray start step dur) =
  Ray (start `addTime` time) step dur

-- XXX
-- offsetRay :: Rational -> Ray d -> Ray d
-- offsetRay x (Ray c d k p) = Ray (c + x) d k p

-- splitRay :: Rational -> Ray d -> (Ray d, Ray d)
-- splitRay x r = (cutRay (Duration x) r, offsetRay (-x) (omitRay x r))

-- -- Assume d is Finite.
-- reverseRay :: Ray d -> Ray d
-- reverseRay (Ray c dur k p) = Ray (c + d * signum k) (Duration d) (-k) p'
--   where
--     p' = abs (d - p) `rmod` abs k

-- stretchRay :: (LinearDuration d, Fractional d) => Rational -> Ray d -> Ray d
-- stretchRay scale (Ray start step dur) =
--   Ray (start * fromRational scale) (step * fromRational scale) (dur * fromRational scale)

-- c ((/ r) <$> d) (k / r) (p / r)

-- Check whether the given rational is contained in the ray
-- onRay :: TimeType d -> Ray d -> Bool
-- onRay t (Ray start step dur) =
--   -- check sign of k.
--   --   - if k > 0  then  c <= x <= c + d
--   --   - otherwise       c - d <= x <= c
--   -- also need x == c + p + kt for some integer t.
--   --   hence compute (x - c - p) / k  and check whether it is integer.
--   upperBound && lowerBound && (denominator ((x - c - p) / k) == 1)
--   where
--     upperBound = case d of
--       Duration d'
--         | k > 0 -> x <= c + d'
--         | k < 0 -> c - d' <= x
--       Forever -> True
--     lowerBound
--       | k > 0 = c <= x
--       | k < 0 = x <= c
