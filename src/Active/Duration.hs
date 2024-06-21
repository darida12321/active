{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Active.Duration
-- Copyright   :  (c) 2017 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@gmail.com
--
-- Finite and infinite durations.
module Active.Duration
  ( -- * Duration
    Time,
    Duration,
    LinearDuration,
    TimeType,
    InfiniteDuration,

    -- * Time
    nullTime,
    unitTime,
    smallerThanTime,
    addTime,
    subTime,

    -- * Duration
    nullDuration,
    smallerThanDur,
    containsTime,
    fromTime,
    removeTime,
    compose,
    timeAtEnd,

    -- * Linear duration
    reverseDur,
    endTime,
    addDur,

    -- * Infinite duration
    infiniteDur,
  )
where

class Time t where
  nullTime :: t
  unitTime :: t
  smallerThanTime :: t -> t -> Bool

  addTime :: t -> t -> t
  subTime :: t -> t -> Maybe t

class (Time (TimeType d)) => Duration d where
  type TimeType d :: *
  nullDuration :: d

  smallerThanDur :: d -> d -> Bool
  containsTime :: d -> TimeType d -> Bool
  timeAtEnd :: d -> TimeType d -> Bool

  fromTime :: TimeType d -> d
  removeTime :: d -> TimeType d -> d

  compose :: d -> TimeType d -> d -> Maybe d

class (Duration d, Ord d) => LinearDuration d where
  reverseDur :: d -> TimeType d -> Maybe (TimeType d)
  endTime :: d -> Maybe (TimeType d)
  addDur :: d -> d -> d

class (Duration d) => InfiniteDuration d where
  infiniteDur :: d

--

-- Rational
instance {-# OVERLAPPABLE #-} (Num a, Ord a) => Time a where
  nullTime = 0
  unitTime = 1
  smallerThanTime = (<)

  addTime = (+)
  subTime t1 t2
    | t1 < t2 = Nothing
    | otherwise = Just (t1 - t2)

instance Duration Rational where
  type TimeType Rational = Rational
  nullDuration = 0

  smallerThanDur = (<)
  containsTime = (<)

  fromTime = id
  removeTime d t
    | d < t = error "TODO"
    | otherwise = d - t

  compose d1 t d2
    | t == d1 = Just $ d1 + d2
    | otherwise = Nothing

  timeAtEnd d t = d == t

instance InfiniteDuration Rational where
  infiniteDur = 1000 -- TODO change to dur

instance LinearDuration Rational where
  reverseDur d t
    | d < t = Nothing
    | otherwise = Just $ d - t
  addDur d1 d2 = d1 + d2
  endTime d = Nothing

--

-- -- List of events changing the state of the animation
-- data EventTime e = EventTime [e] Double

-- instance (Eq e) => Time (EventTime e) where
--   nullTime = EventTime [] 0

--   smallerThanTime (EventTime [] t1) (EventTime [] t2) = t1 < t2
--   smallerThanTime (EventTime [] _) (EventTime _ _) = True
--   smallerThanTime (EventTime _ _) (EventTime [] _) = False
--   smallerThanTime (EventTime (e1 : es1) t1) (EventTime (e2 : es2) t2)
--     | e1 == e2 = smallerThanTime (EventTime es1 t1) (EventTime es2 t2)
--     | otherwise = False

--   addTime (EventTime es1 t1) (EventTime es2 t2) =
--     EventTime (es1 <> es2) t2

-- newtype EventDur e = EventDur [(e, EventDur e)] deriving (Eq)

-- instance (Eq e) => Duration (EventDur e) where
--   type TimeType (EventDur e) = EventTime e

--   nullDuration = EventDur []

--   containsTime (EventDur eds) (EventTime [] t) = True
--   containsTime (EventDur eds) (EventTime (e : es) t)
--     | e `elem` map fst eds = containsTime dur (EventTime es t)
--     | otherwise = False
--     where
--       dur = snd $ head $ filter (\x -> e == fst x) eds

--   smallerThanDur (EventDur []) (EventDur []) = True
--   smallerThanDur (EventDur _) (EventDur []) = True
--   smallerThanDur (EventDur []) (EventDur _) = False
--   smallerThanDur e1@(EventDur (ed1 : eds1)) e2@(EventDur eds2)
--     | e1 == e2 = True
--     | otherwise = False -- TODO finish this with helper func

--   fromTime (EventTime [] t) = EventDur []
--   fromTime (EventTime (e : es) t) =
--     EventDur [(e, fromTime (EventTime es t))]

--   removeTime ed (EventTime [] t) = ed
--   removeTime (EventDur eds) (EventTime (e : es) t)
--     | e `elem` map fst eds = removeTime dur (EventTime es t)
--     | otherwise = error "Time is outside of this duration."
--     where
--       dur = snd $ head $ filter (\x -> e == fst x) eds

--   compose (EventDur eds1) (EventTime [] t) (EventDur eds2)
--     | any (uncurry (==)) allPairs = Nothing
--     | otherwise = Just $ EventDur (eds1 <> eds2)
--     where
--       allPairs = [(e1, e2) | e1 <- map fst eds1 | e2 <- map fst eds2]
--   compose (EventDur eds1) (EventTime [e] t) dur2
--     | e `notElem` map fst eds1 = Just $ EventDur ([(e, dur2)] <> eds1)
--   compose (EventDur eds1) (EventTime (e : es) t) dur2
--     | e `notElem` map fst eds1 = Nothing
--     | otherwise = case composedDur of
--         Just composedDur' -> Just $ EventDur ([(e, composedDur')] <> eds1)
--         Nothing -> Nothing
--     where
--       replacedDur = snd $ head $ filter (\x -> e == fst x) eds1
--       composedDur = compose replacedDur (EventTime es t) dur2
--       withoutBranch = filter (\x -> e /= fst x) eds1

-- -- Example playground
-- data Keys = KLeft | KRight deriving (Eq)

-- branchExplore :: EventDur Keys
-- branchExplore = EventDur [(KLeft, leftDur), (KRight, rightDur)]
--   where
--     leftDur = EventDur []
--     rightDur = EventDur []

-- branchExploreTime :: EventTime Keys
-- branchExploreTime = EventTime [KRight] 42

--
--
--

-- is eq really needed?
-- endPoints :: d -> [TimeType d] what if infinite??

-- take only first bit    DONE
-- drop the first bit     DONE
-- stretch by duration    only num instances?

-- Make Rational an instance of this
-- Make Int an instance of this

-- import Linear.Vector

-- #if !MIN_VERSION_base(4,8,0)
-- import           Control.Applicative
-- #endif

-- ------------------------------------------------------------
-- -- Durations
-- ------------------------------------------------------------

-- -- | The type of (potentially infinite) /durations/ over a given
-- --   numeric type @n@. The infinite duration is longer than any finite
-- --   duration.
-- data Duration :: * -> * where
--   -- | A finite duration of a given nonnegative length.  The length
--   --   can be zero.
--   Duration :: n -> Duration n
--   -- | An infinite duration.
--   Forever :: Duration n
--   deriving (Show, Eq, Ord, Functor)

-- -- | @Duration Rational@ is common enough that it's worth giving it a
-- --   short type synonym for convenience.
-- type Dur = Duration Rational

-- instance Applicative Duration where
--   pure = Duration
--   Forever <*> _ = Forever
--   _ <*> Forever = Forever
--   Duration f <*> Duration x = Duration (f x)

-- -- | Durations inherit the additive structure of the underlying
-- --   numeric type; the sum of the infinite duration with anything is
-- --   infinite. Note that it does not make sense to multiply durations,
-- --   but you can scale them by a constant using the ('*^') operator
-- --   from the 'Additive' instance.
-- --
-- --   This instance also gives us the convenience of 'fromInteger', so
-- --   numeric literals can be used as finite durations.
-- instance (Num n) => Num (Duration n) where
--   fromInteger = toDuration . fromInteger

--   Forever + _ = Forever
--   _ + Forever = Forever
--   Duration d1 + Duration d2 = Duration (d1 + d2)

--   abs Forever = Forever
--   abs (Duration n) = Duration (abs n)

--   (*) = error "Multiplying durations makes no sense. Use (*^) to scale by a constant."
--   negate = error "Negating durations makes no sense."
--   signum = error "Signum on durations makes no sense."

-- instance Additive Duration where
--   zero = Duration 0

-- isForever :: Duration n -> Bool
-- isForever Forever = True
-- isForever _ = False

-- -- | A wrapper function to convert a numeric value into a finite duration.
-- toDuration :: n -> Duration n
-- toDuration = Duration

-- -- | An unwrapper function to turn a duration into a numeric value.
-- --   Finite durations become @Just@; the infinite duration becomes
-- --   @Nothing@.
-- fromDuration :: Duration n -> Maybe n
-- fromDuration Forever = Nothing
-- fromDuration (Duration n) = Just n

-- -- | Subtract a finite duration from another duration.  If the first
-- --   duration is infinite, the result is also infinite.  If the second
-- --   duration is longer than the first, the result is zero.
-- subDuration :: (Num n, Ord n) => Duration n -> Duration n -> Duration n
-- subDuration Forever _ = Forever
-- subDuration (Duration a) (Duration b) | b <= a = Duration (a - b)
-- subDuration _ _ = Duration 0
