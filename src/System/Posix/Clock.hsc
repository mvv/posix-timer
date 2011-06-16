{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | POSIX clocks.
module System.Posix.Clock (
    TimeSpec,
    timeSpecSeconds,
    timeSpecNanoseconds,
    mkTimeSpec,
    timeSpecV,
    timeSpecToNum,
    timeSpecToInt64,

    Clock(..),
    monotonicClock,
    realtimeClock,
    processTimeClock,
    threadTimeClock,

    getProcClock,
    getClockResolution,
    getClockTime,
    setClockTime,
    clockSleep,
    clockSleepAbs,
  ) where

import Data.Int
import Data.Word
import Data.Ratio (numerator)
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

nsPerSecond :: Num a => a
nsPerSecond = 1000000000
{-# INLINE nsPerSecond #-}

-- | Mirrors /struct timespec/.
data TimeSpec = TimeSpec { timeSpecSeconds :: CTime
                         , timeSpecNanoseconds :: CULong
                         } deriving (Eq, Show)

-- | Create a 'TimeSpec' from amounts of seconds and nanoseconds.
mkTimeSpec :: CTime -> CULong -> TimeSpec
mkTimeSpec seconds nanoseconds =
  case nanoseconds of
    ns | ns >= nsPerSecond ->
      TimeSpec (seconds + (fromIntegral q)) r
        where (q, r) = ns `quotRem` nsPerSecond 
    _ -> TimeSpec seconds nanoseconds

-- | Convert a 'TimeSpec' to a pair of its components.
--   Useful as a view pattern.
timeSpecV :: TimeSpec -> (CTime, CULong)
timeSpecV (TimeSpec s ns) = (s, ns)
{-# INLINE timeSpecV #-}

-- | The total amount of time a 'TimeSpec' represents,
--   in nanoseconds.
timeSpecToNum :: Num a => TimeSpec -> a
timeSpecToNum = fromInteger . numerator . toRational
{-# RULES
"timeSpecToNum/Int64"  timeSpecToNum = timeSpecToInt64
"timeSpecToNum/Word64" timeSpecToNum = \x -> fromIntegral (timeSpecToInt64 x)
  #-}

-- | Specialized version of 'timeSpecToNum'.
timeSpecToInt64 :: TimeSpec -> Int64
timeSpecToInt64 (TimeSpec s ns) =
  let ns64 = fromIntegral ns in
#if HTYPE_TIME_T == Int64
    (unsafeCoerce s :: Int64) * nsPerSecond +
#elif HTYPE_TIME_T == Int32
    (fromIntegral (unsafeCoerce s :: Int32) :: Int64) * nsPerSecond +
#else
# error FIXME: timeSpecToInt64: unexpected HTYPE_TIME_T
#endif
    if s >= 0 then ns64 else -ns64

instance Ord TimeSpec where
  (TimeSpec s1 ns1) `compare` (TimeSpec s2 ns2) = 
    case s1 `compare` s2 of
      EQ -> ns1 `compare` ns2
      x -> x

instance Bounded TimeSpec where
  minBound = TimeSpec (fromIntegral (minBound :: #{itype clock_t})) 0
  maxBound = TimeSpec (fromIntegral (maxBound :: #{itype clock_t}))
                      (nsPerSecond - 1)

instance Num TimeSpec where
  (TimeSpec s1 ns1) * (TimeSpec s2 ns2) =
    mkTimeSpec (s1 * s2 * nsPerSecond +
                s1 * (fromIntegral ns2) + s2 * (fromIntegral ns1) +
                (fromIntegral q)) $ fromIntegral r
      where (q, r) = ((fromIntegral ns1 :: Word64) *
                      (fromIntegral ns2 :: Word64)) `quotRem` nsPerSecond
  (TimeSpec s1 ns1) + (TimeSpec s2 ns2) = mkTimeSpec (s1 + s2) (ns1 + ns2)
  (TimeSpec s1 ns1) - (TimeSpec s2 ns2) =
    if ns1 < ns2 then TimeSpec (s1 - s2 - 1) (nsPerSecond - ns2 + ns1)
                 else TimeSpec (s1 - s2) (ns1 - ns2)
  negate (TimeSpec s ns) = mkTimeSpec ((-s) - 1) (nsPerSecond - ns)
  abs ts@(TimeSpec s _) = if s >= 0 then ts else negate ts
  signum (TimeSpec s _) =
    TimeSpec 0 (if s < 0 then -1 else (if s == 0 then 0 else nsPerSecond -1))
  fromInteger i = TimeSpec (fromInteger s) (fromInteger ns)
                    where (s, ns) = i `divMod` nsPerSecond

instance Real TimeSpec where
  toRational (TimeSpec s ns) =
    let rns = toRational ns in
      toRational s * nsPerSecond + if s >= 0 then rns else -rns

instance Storable TimeSpec where
  alignment _ = #{alignment struct timespec}
  sizeOf _ = #{size struct timespec}
  peek p = do
    seconds <- #{peek struct timespec, tv_sec} p
    nanoseconds <- #{peek struct timespec, tv_nsec} p
    return $ TimeSpec seconds nanoseconds
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
getProcClock :: ProcessID -> IO Clock
getProcClock pid = do
  alloca $ \p -> do
    throwErrnoIfMinus1_ "getProcClock" $ c_clock_getcpuclockid pid p
    peek p

-- | Get the clock resolution. See /clock_getres(3)/.
getClockResolution :: Clock -> IO TimeSpec
getClockResolution clock = do
  alloca $ \p -> do
    throwErrnoIfMinus1_ "getClockResolution" $ c_clock_getres clock p
    peek p

-- | Get the clock time. See /clock_gettime(3)/.
getClockTime :: Clock -> IO TimeSpec
getClockTime clock = do
  alloca $ \p -> do
    throwErrnoIfMinus1_ "getClockTime" $ c_clock_gettime clock p
    peek p

-- | Set the clock time. See /clock_settime(3)/.
setClockTime :: Clock -> TimeSpec -> IO ()
setClockTime clock ts =
  with ts $ throwErrnoIfMinus1_ "setClockTime" . c_clock_settime clock

-- | Sleep for the specified duration. When interrupted by a signal, returns
--   the amount of time left to sleep. See /clock_nanosleep(3)/.
clockSleep :: Clock -> TimeSpec -> IO TimeSpec
clockSleep clock ts =
  with ts $ \pTs ->
    alloca $ \pLeft -> do 
      result <- c_clock_nanosleep clock 0 pTs pLeft
      if result == 0
        then return 0
        else do
          errno <- getErrno
          if errno == eINTR
            then peek pLeft
            else throwErrno "clockSleep"

-- | Sleep until the clock time reaches the specified value.
--   See /clock_nanosleep(3)/.
clockSleepAbs :: Clock -> TimeSpec -> IO ()
clockSleepAbs clock ts =
  with ts $ \p ->
    throwErrnoIfMinus1_ "clockSleepAbs" $
      c_clock_nanosleep clock #{const TIMER_ABSTIME} p nullPtr

foreign import ccall unsafe "clock_getcpuclockid"
  c_clock_getcpuclockid :: ProcessID -> Ptr Clock -> IO CInt
foreign import ccall unsafe "clock_getres"
  c_clock_getres :: Clock -> Ptr TimeSpec -> IO CInt
foreign import ccall unsafe "clock_gettime"
  c_clock_gettime :: Clock -> Ptr TimeSpec -> IO CInt
foreign import ccall unsafe "clock_settime"
  c_clock_settime :: Clock -> Ptr TimeSpec -> IO CInt
foreign import ccall unsafe "clock_nanosleep"
  c_clock_nanosleep :: Clock -> CInt -> Ptr TimeSpec -> Ptr TimeSpec -> IO CInt

