{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

-- | POSIX timers.
module System.Posix.Timer (
    ITimerSpec(..),
    Timer,
    createTimer,
    configureTimer,
    timerTimeLeft,
    timerOverrunCnt,
    destroyTimer
  ) where

import Data.Word
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Base
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, WordPtr, nullPtr, castPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (with)
#if __GLASGOW_HASKELL__ >= 703
import Foreign.C.Types (CInt(..))
#else
import Foreign.C.Types (CInt)
#endif
import Foreign.C.Error (throwErrnoIfMinus1, throwErrnoIfMinus1_)
import System.Posix.Signals (Signal)
import System.Posix.Clock (TimeSpec, Clock(..))

#include <time.h>
#include <signal.h>
#include <posix-timer.macros.h>

#let alignment t = "%lu", (unsigned long) offsetof (struct { char x__; t (y__); }, y__)

-- | Mirrors /struct itimerspec/.
data ITimerSpec = ITimerSpec { iTimerSpecInterval ∷ !TimeSpec
                             , iTimerSpecValue    ∷ !TimeSpec
                             } deriving (Eq, Show)

instance Storable ITimerSpec where
  alignment _ = #{alignment struct itimerspec}
  sizeOf _    = #{size struct itimerspec}
  peek p = ITimerSpec <$> #{peek struct itimerspec, it_interval} p
                      <*> #{peek struct itimerspec, it_value} p
  poke p (ITimerSpec interval value) = do
    #{poke struct itimerspec, it_interval} p interval
    #{poke struct itimerspec, it_value} p value

-- | Mirrors /timer_t/.
newtype Timer = Timer #{itype timer_t} deriving (Eq, Ord, Show, Storable)

-- | Create a timer. See /timer_create(3)/.
createTimer ∷ MonadBase IO μ
            ⇒ Clock
            → Maybe (Signal, WordPtr) -- ^ Optional signal to raise on timer
                                      --   expirations and value of
                                      --   /siginfo.si_value/.
            → μ Timer
createTimer clock sigEvent =
  liftBase $ alloca $ \pTimer → do
    throwErrnoIfMinus1_ "createTimer" $
      case sigEvent of
        Just (signal, ud) → do
          allocaBytes #{size struct sigevent} $ \pEv → do
            #{poke struct sigevent, sigev_notify} pEv
              (#{const SIGEV_SIGNAL} ∷ CInt)
            #{poke struct sigevent, sigev_signo} pEv signal
            #{poke struct sigevent, sigev_value} pEv ud
            c_timer_create clock (castPtr $ (pEv ∷ Ptr Word8)) pTimer
        Nothing →
          c_timer_create clock nullPtr pTimer
    peek pTimer

-- | Setup the timer. See /timer_settime(3)/.
configureTimer ∷ MonadBase IO μ
               ⇒ Timer
               → Bool -- ^ Whether the expiration time is absolute.
               → TimeSpec -- ^ Expiration time. Zero value disarms the timer.
               → TimeSpec -- ^ Interval between subsequent expirations.
               → μ (TimeSpec, TimeSpec)
configureTimer timer absolute value interval =
  liftBase $ with (ITimerSpec interval value) $ \pNew →
    alloca $ \pOld → do
      throwErrnoIfMinus1_ "configureTimer" $
        c_timer_settime timer
          (if absolute then #{const TIMER_ABSTIME} else 0) pNew pOld
      ITimerSpec oldInterval oldValue ← peek pOld
      return (oldValue, oldInterval)

-- | Get the amount of time left until the next expiration and the interval
--   between the subsequent expirations. See /timer_gettime(3)/.
timerTimeLeft ∷ MonadBase IO μ ⇒ Timer → μ (TimeSpec, TimeSpec)
timerTimeLeft timer =
  liftBase $ alloca $ \p → do
    throwErrnoIfMinus1_ "timerTimeLeft" $ c_timer_gettime timer p
    ITimerSpec interval value ← peek p
    return (value, interval)

-- | Get the timer overrun count. See /timer_getoverrun(3)/.
timerOverrunCnt ∷ MonadBase IO μ ⇒ Timer → μ CInt
timerOverrunCnt timer =
  liftBase $ throwErrnoIfMinus1 "timerOverrunCnt" $ c_timer_getoverrun timer

-- | Destroy the timer. See /timer_delete(3)/.
destroyTimer ∷ MonadBase IO μ ⇒ Timer → μ ()
destroyTimer timer =
  liftBase $ throwErrnoIfMinus1_ "deleteTimer" $ c_timer_delete timer

foreign import ccall unsafe "timer_create"
  c_timer_create ∷ Clock → Ptr () → Ptr Timer → IO CInt
foreign import ccall unsafe "timer_settime"
  c_timer_settime ∷
    Timer → CInt → Ptr ITimerSpec → Ptr ITimerSpec → IO CInt
foreign import ccall unsafe "timer_gettime"
  c_timer_gettime ∷ Timer → Ptr ITimerSpec → IO CInt
foreign import ccall unsafe "timer_getoverrun"
  c_timer_getoverrun ∷ Timer → IO CInt
foreign import ccall unsafe "timer_delete"
  c_timer_delete ∷ Timer → IO CInt

