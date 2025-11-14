module Web.Async.Example.Performance

import Data.Either
import Data.List.TR
import Data.Nat
import Data.Refined.Integer
import Data.String

import Derive.Prelude
import Derive.Literal

import Web.Async.Example.CSS.Performance
import Web.Async.Example.Util

%default total
%language ElabReflection

MinBtns, MaxBtns : Integer
MinBtns  = 1
MaxBtns  = 100_000

record NumBtns where
  constructor B
  value : Integer
  {auto 0 prf : FromTo MinBtns MaxBtns value}

%runElab derive "NumBtns" [Show,Eq,Ord,IntegerLit]

data PerfEv : Type where
  PerfInit   : PerfEv
  NumChanged : Either String NumBtns -> PerfEv
  Reload     : PerfEv
  Clicked    : Nat -> PerfEv

read : String -> Either String NumBtns
read =
  let err := "expected integer between \{show MinBtns} and \{show MaxBtns}"
   in maybeToEither err . refineNumBtns . cast

record PerfST where
  constructor P
  sum : Nat
  num : Maybe NumBtns

init : PerfST
init = P 0 Nothing

btnRef : Nat -> Ref Tag.Button
btnRef n = Id "BTN\{show n}"

parameters {auto ev : Sink PerfEv}

  btn : Nat -> HTMLNode
  btn n =
    button
      [Id (btnRef n), onClick (Clicked n), classes [widget,btn,inc]]
      [Text $ show n]

  btns : NumBtns -> HTMLNode
  btns (B n) = div [class grid] $ map btn (iterateTR (cast n) S 1)

  content : HTMLNode
  content =
    div [ class performanceContent ]
      [ lbl "Number of buttons:" numButtonsLbl
      , input
          [ Id natIn
          , onInput (NumChanged . read)
          , onEnterDown Reload
          , classes [widget, textIn]
          , placeholder "Enter a positive integer"
          ]
      , button [Id btnRun, onClick Reload, classes [widget, btn]] ["Run"]
      , lbl "Sum:" sumLbl
      , div [Id out] []
      , div [Id time] []
      , div [Id buttons] []
      ]

update : PerfEv -> PerfST -> PerfST
update PerfInit       = const init
update (NumChanged e) = {num := eitherToMaybe e}
update Reload         = {sum := 0, num := Nothing}
update (Clicked k)    = {sum $= (+k)}

dispTime : NumBtns -> Integer -> String
dispTime 1 ms = "\Loaded one button in \{show ms} ms."
dispTime n ms = "\Loaded \{show n.value} buttons in \{show ms} ms."

export
run : JSStream Void
run =
  mvcAct PerfInit init $ \e,s => do
    case e of
      PerfInit     => child exampleDiv content
      NumChanged e => validate natIn e
      Clicked k    => disabled (btnRef k) True
      Reload       => do
        setValue natIn ""
        for_ s.num $ \v =>
          timed' (child buttons $ btns v) >>= text time . dispTime v
    let s2 := update e s
    disabledM btnRun s2.num
    show out s2.sum
    pure s2
