module Web.Async.Example.Reset

import Web.Async.Example.CSS.Reset
import Web.Async.Example.Util

%default total

data ResetEv : Type where
  ResetInit : ResetEv
  Mod       : (Int8 -> Int8) -> ResetEv

btn : Sink ResetEv => Ref Tag.Button -> (Int8 -> Int8) -> String -> HTMLNode
btn r ev l = button [Id r, onClick (Mod ev), classes [widget,btn]] [Text l]

content : Sink ResetEv => HTMLNode
content =
  div
    [ class resetContent ]
    [ lbl "Reset counter:"    resetLbl, btn btnReset (const 0) "Reset"
    , lbl "Increase counter:" incLbl,   btn btnInc   (+ 1)     "+"
    , lbl "Decrease counter:" decLbl,   btn btnDec   (+ (-1))  "-"
    , lbl "Count:"            countLbl, div [Id out] []
    ]

update : ResetEv -> Int8 -> Int8
update ResetInit n = 0
update (Mod f)   n = f n

parameters {auto ev  : Sink ResetEv}

  displayST : Int8 -> Act ()
  displayST n = do
    disabled btnDec   (n <= -10)
    disabled btnInc   (n >= 10)
    disabled btnReset (n == 0)
    show out n

  display : ResetEv -> Int8 -> Act ()
  display ResetInit n = child exampleDiv content >> displayST n
  display (Mod f)   n = displayST n

export
run : Prog Void ()
run = mvc ResetInit 0 update display
