{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

-- | POSIX clocks.
module System.Posix.Clock (
    TimeSpec,
    timeSpecSeconds,
    timeSpecNanos,
    mkTimeSpec,
    timeSpecV,
    timeSpecToInt64,

    Clock(..),
    monotonicClock,
    realtimeClock,
    processTimeClock,
    threadTimeClock,

    getProcessClock,
    getClockResolution,
    getClockTime,
    setClockTime,
    clockSleep,
    clockSleepAbs,
  ) where

import Data.Int
import Data.Word
import Data.Ratio (numerator)
import Data.List (unfoldr)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Base
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.C.Types (CInt, CULong, CTime)
import Foreign.C.Error (getErrno, eINTR, throwErrno, throwErrnoIfMinus1_)
import System.Posix.Types (ProcessID)
import Unsafe.Coerce (unsafeCoerce)

#include <time.h>
#include <HsBaseConfig.h>
#include <posix-timer.macros.h>

#let alignment t = "%lu", (unsigned long) offsetof (struct { char x__; t (y__); }, y__)

nsPerSecond ∷ Num α ⇒ α
nsPerSecond = 1000000000
{-# INLINE nsPerSecond #-}

minSecsInInt, minNsInInt ∷ Int
(minSecsInInt, minNsInInt) = (minBound ∷ Int) `divMod` nsPerSecond

maxSecsInInt, maxNsInInt ∷ Int
(maxSecsInInt, maxNsInInt) = (maxBound ∷ Int) `divMod` nsPerSecond

-- | Mirrors /struct timespec/.
data TimeSpec = TimeSpec { timeSpecSeconds ∷ CTime
                         , timeSpecNanos   ∷ CULong
                         } deriving (Eq, Show)

-- | Create a 'TimeSpec' from amounts of seconds and nanoseconds.
mkTimeSpec ∷ CTime → CULong → TimeSpec
mkTimeSpec s ns | ns < nsPerSecond = TimeSpec s ns
                | otherwise        = TimeSpec (s + fromIntegral q) r
  where (q, r) = ns `quotRem` nsPerSecond 

-- | Convert a 'TimeSpec' to a pair of its components.
--   Useful as a view pattern.
timeSpecV ∷ TimeSpec → (CTime, CULong)
timeSpecV (TimeSpec s ns) = (s, ns)
{-# INLINE timeSpecV #-}

instance Ord TimeSpec where
  (TimeSpec s1 ns1) `compare` (TimeSpec s2 ns2) = 
    case s1 `compare` s2 of
      EQ → ns1 `compare` ns2
      x  → x

instance Bounded TimeSpec where
  minBound = TimeSpec (fromIntegral (minBound ∷ #{itype time_t})) 0
  maxBound = TimeSpec (fromIntegral (maxBound ∷ #{itype time_t}))
                      (nsPerSecond - 1)

instance Num TimeSpec where
  (TimeSpec s1 ns1) * (TimeSpec s2 ns2) =
    mkTimeSpec (s1 * s2 * nsPerSecond +
                s1 * (fromIntegral ns2) + s2 * (fromIntegral ns1) +
                (fromIntegral q)) $ fromIntegral r
      where (q, r) = ((fromIntegral ns1 ∷ Word64) *
                      (fromIntegral ns2 ∷ Word64)) `quotRem` nsPerSecond
  (TimeSpec s1 ns1) + (TimeSpec s2 ns2) = mkTimeSpec (s1 + s2) (ns1 + ns2)
  (TimeSpec s1 ns1) - (TimeSpec s2 ns2) =
    if ns1 < ns2 then TimeSpec (s1 - s2 - 1) (nsPerSecond - ns2 + ns1)
                 else TimeSpec (s1 - s2) (ns1 - ns2)
  negate (TimeSpec s ns) = mkTimeSpec ((-s) - 1) (nsPerSecond - ns)
  abs ts@(TimeSpec s _) = if s >= 0 then ts else negate ts
  signum (TimeSpec s ns) | s < 0     = TimeSpec (-1) (nsPerSecond - 1)
                         | otherwise = TimeSpec 0 $ signum ns
  fromInteger i = TimeSpec (fromInteger s) (fromInteger ns)
                    where (s, ns) = i `divMod` nsPerSecond

instance Real TimeSpec where
  toRational (TimeSpec s ns) = toRational s * nsPerSecond + toRational ns

instance Enum TimeSpec where
  succ (TimeSpec s ns) | ns == nsPerSecond - 1 = TimeSpec (succ s) 0
                       | otherwise             = TimeSpec s (succ ns)
  pred (TimeSpec s ns) | ns == 0   = TimeSpec (pred s) (nsPerSecond - 1)
                       | otherwise = TimeSpec s (ns - 1)
  toEnum i = TimeSpec (fromIntegral s) (fromIntegral ns)
               where (s, ns) = i `divMod` nsPerSecond
  fromEnum (TimeSpec s ns) =
      if s' < minSecs || (s' == minSecs && ns < minNs) ||
         s' > maxSecs || (s' == maxSecs && ns > maxNs)
        then error "TimeSpec.fromEnum"
        else fromIntegral s' * nsPerSecond + fromIntegral ns
    where s', minSecs, maxSecs ∷ #{itype time_t}
          s' = unsafeCoerce s
          minSecs = fromIntegral minSecsInInt
          maxSecs = fromIntegral maxSecsInInt
          minNs, maxNs ∷ CULong
          minNs = fromIntegral minNsInInt
          maxNs = fromIntegral maxNsInInt
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where bound | y >= x    = maxBound
                | otherwise = minBound
  enumFromTo x y
    | y >= x    = unfoldr (\z → if z == y then Nothing else Just (z, z + 1)) x
    | otherwise = unfoldr (\z → if z == y then Nothing else Just (z, z - 1)) x
  enumFromThenTo x n y
      | d >= 0    =
          if y < x
            then []
            else unfoldr (\z → if z > y then Nothing else Just (z, z + d)) x
      | otherwise =
          if y > x
            then []
            else unfoldr (\z → if z < y then Nothing else Just (z, z - d)) x
    where d = n - x

instance Integral TimeSpec where
  toInteger (TimeSpec s ns) = (numerator $ toRational s) * nsPerSecond +
                              toInteger ns
  -- TODO: Make it faster when the arguments are small enough
  quotRem tsN tsD = (fromInteger q, fromInteger r)
    where (q, r) = quotRem (toInteger tsN) (toInteger tsD)

timeSpecToInt64 ∷ TimeSpec → Int64
timeSpecToInt64 (TimeSpec s ns) =
#if HTYPE_TIME_T == Int64
  (unsafeCoerce s ∷ Int64) * nsPerSecond +
#elif HTYPE_TIME_T == Int32
  (fromIntegral (unsafeCoerce s ∷ Int32) ∷ Int64) * nsPerSecond +
#else
# error FIXME: timeSpecToInt64: unexpected HTYPE_TIME_T value
#endif
  fromIntegral ns
{-# INLINE timeSpecToInt64 #-}

{-# RULES
"fromIntegral/TimeSpec->Int64"  fromIntegral = timeSpecToInt64
"fromIntegral/TimeSpec->Word64" fromIntegral = fromIntegral . timeSpecToInt64
  #-}

instance Storable TimeSpec where
  alignment _ = #{alignment struct timespec}
  sizeOf _    = #{size struct timespec}
  peek p = TimeSpec <$> #{peek struct timespec, tv_sec} p
                    <*> #{peek struct timespec, tv_nsec} p
  poke p (TimeSpec seconds nanoseconds) = do
    #{poke struct timespec, tv_sec} p seconds
    #{poke struct timespec, tv_nsec} p nanoseconds

-- | Mirrors /clockid_t/.
newtype Clock = Clock #{itype clockid_t} deriving (Eq, Ord, Show, Storable)

#{enum Clock, Clock
 , monotonicClock = CLOCK_MONOTONIC
 , realtimeClock = CLOCK_REALTIME
 , processTimeClock = CLOCK_PROCESS_CPUTIME_ID
 , threadTimeClock = CLOCK_THREAD_CPUTIME_ID
 }

-- | Get the CPU-time clock of the given process.
--   See /clock_getcpuclockid(3)/.
getProcessClock ∷ MonadBase μ IO ⇒ ProcessID → μ Clock
getProcessClock pid =
  liftBase $ alloca $ \p → do
    throwErrnoIfMinus1_ "getProcClock" $ c_clock_getcpuclockid pid p
    peek p

-- | Get the clock resolution. See /clock_getres(3)/.
getClockResolution ∷ MonadBase μ IO ⇒ Clock → μ TimeSpec
getClockResolution clock =
  liftBase $ alloca $ \p → do
    throwErrnoIfMinus1_ "getClockResolution" $ c_clock_getres clock p
    peek p

-- | Get the clock time. See /clock_gettime(3)/.
getClockTime ∷ MonadBase μ IO ⇒ Clock → μ TimeSpec
getClockTime clock =
  liftBase $ alloca $ \p → do
    throwErrnoIfMinus1_ "getClockTime" $ c_clock_gettime clock p
    peek p

-- | Set the clock time. See /clock_settime(3)/.
setClockTime ∷ MonadBase μ IO ⇒ Clock → TimeSpec → μ ()
setClockTime clock ts =
  liftBase $ with ts $
    throwErrnoIfMinus1_ "setClockTime" . c_clock_settime clock

-- | Sleep for the specified duration. When interrupted by a signal, returns
--   the amount of time left to sleep. See /clock_nanosleep(3)/.
clockSleep ∷ MonadBase μ IO ⇒ Clock → TimeSpec → μ TimeSpec
clockSleep clock ts =
  liftBase $ with ts $ \pTs →
    alloca $ \pLeft → do 
      result ← c_clock_nanosleep clock 0 pTs pLeft
      if result == 0
        then return 0
        else do
          errno ← getErrno
          if errno == eINTR
            then peek pLeft
            else throwErrno "clockSleep"

-- | Sleep until the clock time reaches the specified value.
--   See /clock_nanosleep(3)/.
clockSleepAbs ∷ MonadBase μ IO ⇒ Clock → TimeSpec → μ ()
clockSleepAbs clock ts =
  liftBase $ with ts $ \p →
    throwErrnoIfMinus1_ "clockSleepAbs" $
      c_clock_nanosleep clock #{const TIMER_ABSTIME} p nullPtr

foreign import ccall unsafe "clock_getcpuclockid"
  c_clock_getcpuclockid ∷ ProcessID → Ptr Clock → IO CInt
foreign import ccall unsafe "clock_getres"
  c_clock_getres ∷ Clock → Ptr TimeSpec → IO CInt
foreign import ccall unsafe "clock_gettime"
  c_clock_gettime ∷ Clock → Ptr TimeSpec → IO CInt
foreign import ccall unsafe "clock_settime"
  c_clock_settime ∷ Clock → Ptr TimeSpec → IO CInt
foreign import ccall "clock_nanosleep"
  c_clock_nanosleep ∷ Clock → CInt → Ptr TimeSpec → Ptr TimeSpec → IO CInt

