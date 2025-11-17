module Web.Async.Confirm

import Derive.Prelude
import Web.Async
import Web.Async.Form

%default total
%language ElabReflection

public export
data ConfirmEv = Cancel | OK

%runElab derive "ConfirmEv" [Show,Eq,Ord]

confirm : ConfirmEv -> EditRes e -> EditRes (Maybe e)
confirm Cancel y = Valid Nothing
confirm OK     y = map Just y

confirmStream :
     Ref Tag.Button
  -> JSStream ConfirmEv
  -> JSStream (EditRes e)
  -> JSStream (EditRes $ Maybe e)
confirmStream ref cs es =
  resource (hold1 $ es |> observe (disabledEdit ref)) $ \esh =>
    zipWith confirm cs esh.stream

||| Wraps and editor in a parent node with buttons (or similar
||| interactive elements) for cancellation and confirmation.
|||
||| The resulting stream will disable the "confirm" button whenever
||| the current state of the input widget is invalid (or missing).
|||
||| The resulting stream fires only `Valid` events: `Nothing` in case
||| of cancellation and `Just v` in case of confirmation.
|||
||| See also `confirmed1` for a version that only ever fires one event
||| at most.
export
confirmed :
     (wrap : HTMLNode -> Act (Ref Tag.Button, Widget ConfirmEv))
  -> Editor e
  -> Editor (Maybe e)
confirmed wrap (E f) =
  E $ \m => do
    W inner es        <- f (join m)
    (btn, W outer cs) <- wrap inner
    pure (W outer $ confirmStream btn cs es)

||| Like `confirmed` but the resulting stream fires only at most one
||| event.
export
confirmed1 :
     (wrap : HTMLNode -> Act (Ref Tag.Button, Widget ConfirmEv))
  -> Editor e
  -> Editor (Maybe e)
confirmed1 wrap = mapEvents (take 1) . confirmed wrap
