||| Utilities not (yet) available from idris2-dom
module Web.Async.Animate

import Data.Linear.Token
import JS
import Syntax.T1
import Web.Async.Util

--------------------------------------------------------------------------------
--          Timers
--------------------------------------------------------------------------------

||| ID used to identify and cancel a running timer.
public export
data IntervalID : Type where [external]

%foreign "browser:lambda:(n,h,w)=>setInterval(() => h(w),n)"
prim__setInterval : Bits32 -> PrimIO () -> PrimIO IntervalID

%foreign "browser:lambda:(i,w)=>clearInterval(i)"
prim__clearInterval : IntervalID -> PrimIO ()

||| Fires the given event every `n` milliseconds.
|||
||| Note: Use `animate` for smoothly running animations.
export
every : (s : Sink e) => e -> (n : Bits32) -> IO1 (IO1 ())
every ev millis t =
  let i # t := ffi (prim__setInterval millis (primRun $ s.sink ev)) t
   in ffi (prim__clearInterval i) # t

--------------------------------------------------------------------------------
--          Animations
--------------------------------------------------------------------------------

%foreign """
         browser:lambda:(cont,h,w)=>{
            let previousTimeStamp;

            function step(timestamp) {
              if (previousTimeStamp === undefined)
                previousTimeStamp = timestamp;
              const dtime = timestamp - previousTimeStamp;
              previousTimeStamp = timestamp;
              if (cont(w)) {
                h(dtime)(w)
                window.requestAnimationFrame(step);
              }
            }

            window.requestAnimationFrame(step);
         }
         """
prim__animate : PrimIO Boolean -> (Bits32 -> PrimIO ()) -> PrimIO ()

||| Alias for a time delta in milliseconds
public export
DTime : Type
DTime = Bits32

||| Repeatedly fires the given event holding the time delta in
||| milliseconds since the last animation step.
|||
||| In addition, synchronously fires an event with a wrapped
||| handle for stopping the animation.
export
animate : (s : Sink e) => (DTime -> e) -> IO1 (IO1 ())
animate ev = T1.do
  ref <- ref1 true
  ffi (prim__animate (primRun $ read1 ref) (primRun . s.sink . ev))
  pure $ write1 ref false
