{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | POSIX clocks and timers.
module System.Posix.Timer (
    -- * Clocks
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

    -- * Timers
    TimeSpec,
    timeSpecSeconds,
    timeSpecNanoseconds,
    mkTimeSpec,
    timeSpecV,
    ITimerSpec(..),

    Timer,
    createTimer,
    configureTimer,
    timerTimeLeft,
    timerOverrunCnt,
    destroyTimer
  ) where

import Data.Int
import Data.Word
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, WordPtr, nullPtr, castPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.C.Types (CInt, CULong, CTime)
import Foreign.C.Error (getErrno, eINTR, throwErrno, throwErrnoIfMinus1,
                        throwErrnoIfMinus1_)
import System.Posix.Types (ProcessID)
import System.Posix.Signals (Signal)

#include <time.h>
#include <signal.h>
#include <posix-timer.macros.h>

nsPerSecond :: Num a => a
nsPerSecond = 1000000000

-- | Mirrors /clockid_t/.
newtype Clock = Clock #{itype clockid_t} deriving (Eq, Ord, Show, Storable)

#{enum Clock, Clock
 , monotonicClock = CLOCK_MONOTONIC
 , realtimeClock = CLOCK_REALTIME
 , processTimeClock = CLOCK_PROCESS_CPUTIME_ID
 , threadTimeClock = CLOCK_THREAD_CPUTIME_ID
 }

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

#let alignment t = "%lu", (unsigned long) offsetof (struct { char x__; t (y__); }, y__)

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

-- | Mirrors /struct itimerspec/.
data ITimerSpec = ITimerSpec { iTimerSpecInterval :: !TimeSpec
                             , iTimerSpecValue :: !TimeSpec
                             } deriving (Eq, Show)

instance Storable ITimerSpec where
  alignment _ = #{alignment struct itimerspec}
  sizeOf _ = #{size struct itimerspec}
  peek p = do
    interval <- #{peek struct itimerspec, it_interval} p
    value <- #{peek struct itimerspec, it_value} p
    return $ ITimerSpec interval value
  poke p (ITimerSpec interval value) = do
    #{poke struct itimerspec, it_interval} p interval
    #{poke struct itimerspec, it_value} p value

-- | Mirrors /timer_t/.
newtype Timer = Timer #{itype timer_t} deriving (Eq, Ord, Show, Storable)

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

-- | Create a timer. See /timer_create(3)/.
createTimer :: Clock
            -> Maybe (Signal, WordPtr) -- ^ Optional signal to raise on timer
                                       --   expirations and value of
                                       --   /siginfo.si_value/.
            -> IO Timer
createTimer clock sigEvent = do
  alloca $ \pTimer -> do
    throwErrnoIfMinus1_ "createTimer" $
      case sigEvent of
        Just (signal, ud) -> do
          allocaBytes #{size struct sigevent} $ \pEv -> do
            #{poke struct sigevent, sigev_notify} pEv
              (#{const SIGEV_SIGNAL} :: CInt)
            #{poke struct sigevent, sigev_signo} pEv signal
            #{poke struct sigevent, sigev_value} pEv ud
            c_timer_create clock (castPtr $ (pEv :: Ptr Word8)) pTimer
        Nothing ->
          c_timer_create clock nullPtr pTimer
    peek pTimer

-- | Setup the timer. See /timer_settime(3)/.
configureTimer :: Timer
               -> Bool -- ^ Whether the expiration time is absolute.
               -> TimeSpec -- ^ Expiration time. Zero value disarms the timer.
               -> TimeSpec -- ^ Interval between subsequent expirations.
               -> IO (TimeSpec, TimeSpec)
configureTimer timer absolute value interval =
  with (ITimerSpec interval value) $ \pNew ->
    alloca $ \pOld -> do
      throwErrnoIfMinus1_ "configureTimer" $
        c_timer_settime timer
          (if absolute then #{const TIMER_ABSTIME} else 0) pNew pOld
      (ITimerSpec oldInterval oldValue) <- peek pOld
      return (oldValue, oldInterval)

-- | Get the amount of time left until the next expiration and the interval
--   between the subsequent expirations. See /timer_gettime(3)/.
timerTimeLeft :: Timer -> IO (TimeSpec, TimeSpec)
timerTimeLeft timer = do
  alloca $ \p -> do
    throwErrnoIfMinus1_ "timerTimeLeft" $ c_timer_gettime timer p
    (ITimerSpec interval value) <- peek p
    return (value, interval)

-- | Get the timer overrun count. See /timer_getoverrun(3)/.
timerOverrunCnt :: Timer -> IO CInt
timerOverrunCnt timer =
  throwErrnoIfMinus1 "timerOverrunCnt" $ c_timer_getoverrun timer

-- | Destroy the timer. See /timer_delete(3)/.
destroyTimer :: Timer -> IO ()
destroyTimer timer = throwErrnoIfMinus1_ "deleteTimer" $ c_timer_delete timer

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

foreign import ccall unsafe "timer_create"
  c_timer_create :: Clock -> Ptr () -> Ptr Timer -> IO CInt
foreign import ccall unsafe "timer_settime"
  c_timer_settime ::
    Timer -> CInt -> Ptr ITimerSpec -> Ptr ITimerSpec -> IO CInt
foreign import ccall unsafe "timer_gettime"
  c_timer_gettime :: Timer -> Ptr ITimerSpec -> IO CInt
foreign import ccall unsafe "timer_getoverrun"
  c_timer_getoverrun :: Timer -> IO CInt
foreign import ccall unsafe "timer_delete"
  c_timer_delete :: Timer -> IO CInt

