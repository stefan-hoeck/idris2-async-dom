module Web.Async.Example.MathGame

import Data.Linear.Traverse1
import Data.List
import Data.Vect
import Derive.Prelude
import Syntax.T1
import Web.Async.Example.CSS.MathGame
import Web.Async.Example.Util
import Web.Canvas

%language ElabReflection
%default total

--------------------------------------------------------------------------------
-- Random number generation
--------------------------------------------------------------------------------

%foreign "javascript:lambda:(max,w)=>Math.floor(Math.random() * max)"
prim__randomBits32 : Bits32 -> PrimIO Bits32

rnd32 : HasIO io => Bits32 -> io Bits32
rnd32 upperBound = primIO (prim__randomBits32 upperBound)

rndRng32 : HasIO io => (lo,hi : Bits32) -> io Bits32
rndRng32 lo hi = (+lo) <$> rnd32 (hi - lo + 1)

rndFin : HasIO io => (n : Nat) -> io (Fin (S n))
rndFin 0       = pure FZ
rndFin k@(S _) = (restrict k . cast) <$> rnd32 (cast k)

rndSelect : HasIO io => {k : Nat} -> Vect (S k) t -> io t
rndSelect xs = pure $ Vect.index !(rndFin k) xs

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

data Language = EN | DE

%runElab derive "Language" [Eq]

data Op = Plus | Minus | Mult

record Calc where
  constructor MkCalc
  x  : Integer
  y  : Integer
  op : Op

record Tile where
  constructor MkTile
  posX    : Bits8
  posY    : Bits8
  calc    : Calc

result : Calc -> Integer
result (MkCalc x y Plus)  = x + y
result (MkCalc x y Minus) = x - y
result (MkCalc x y Mult)  = x * y

dispCalc : Calc -> String
dispCalc (MkCalc x y op) = "\{show x} \{dispOp op} \{show y} = "

  where
    dispOp : Op -> String
    dispOp Plus  = "+"
    dispOp Minus = "-"
    dispOp Mult  = "*"

data MathEv : Type where
  Lang     : Language -> MathEv
  Check    : MathEv
  MathInit : MathEv
  Inp      : String -> MathEv

lang : String -> MathEv
lang "de" = Lang DE
lang _    = Lang EN

data Result : Type where
  Ended   : Result
  Correct : Result
  Wrong   : Calc -> Integer -> Result

record MathST where
  constructor MS
  lang   : Language
  answer : String
  result : Maybe Result
  rows   : Bits8
  wrong  : List Tile
  calcs  : List Tile
  pic    : String

init : MathST
init = MS EN "" Nothing 4 [] [] ""

currentCalc : MathST -> Maybe Calc
currentCalc gs = case gs.calcs of
  t :: _ => Just t.calc
  Nil    => Nothing

pictures : Vect 11 String
pictures = map (\n => "pics/pic\{show n}.jpg") (fromList [S Z .. 11])

style : Result -> Maybe String
style Ended       = Nothing
style Correct     = Just "color : \{green}"
style (Wrong _ _) = Just "color : \{red}"

language : Language -> String
language DE = "Sprache"
language EN = "Language"

german : Language -> String
german DE = "Deutsch"
german EN = "German"

english : Language -> String
english DE = "Englisch"
english EN = "English"

resultStr : Language -> String
resultStr DE = "Resultat"
resultStr EN = "result"

checkAnswerStr : Language -> String
checkAnswerStr DE = "Antwort prüfen"
checkAnswerStr EN = "Check answer"

newGameStr : Language -> String
newGameStr DE = "Neues Spiel"
newGameStr EN = "New game"

reply : Language -> Result -> String
reply EN Ended   = "The game has ended."
reply EN Correct = "Correct!"
reply DE Ended   = "Das Spiel ist vorbei."
reply DE Correct = "Richtig!"
reply EN (Wrong c n) =
     "That's not correct. Your answer was \{show n}. "
  ++ "The correct answer is: \{dispCalc c} \{show $ result c}."
reply DE (Wrong c n) =
     "Leider falsch. Deine Antwort war \{show n}. "
  ++ "Die richtige Antwort ist: \{dispCalc c} \{show $ result c}."

wcanvas : Bits32
wcanvas = 500

content : Sink MathEv => Language -> HTMLNode
content l =
  div [ class mathContent ]
    [ lbl "\{language l}:" lblLang
    , select
        [ Id langIn, classes [widget, selectIn], onChange MathGame.lang]
        [ option [ value "de", selected (l == DE)] [Text $ german l]
        , option [ value "en", selected (l == EN)] [Text $ english l]
        ]

    , div [ Id calc ] []

    , input
        [ Id resultIn
        , onEnterDown Check
        , onInput Inp
        , class widget
        , placeholder (resultStr l)
        ] []

    , button
        [ Id checkBtn
        , onClick Check
        , classes [widget,btn]
        ] [Text $ checkAnswerStr l]

    , button
        [ Id newBtn
        , onClick MathInit
        , classes [widget,btn]
        ] [Text $ newGameStr l]

    , div [ Id out ] []

    , canvas [ Id pic, width wcanvas, height wcanvas ] []
    ]

stuckColor : Color
stuckColor = HSLA 0 0 50 80

upperBound : Bits32
upperBound = 100

randomCalc : HasIO io => io Calc
randomCalc = do
  op   <- rndSelect [Plus,Minus,Mult]
  case op of
    Plus => do
      x <- rndRng32 0 upperBound
      y <- rndRng32 0 (upperBound - x)
      pure $ MkCalc (cast x) (cast y) op

    Minus => do
      x <- rndRng32 0 upperBound
      y <- rndRng32 0 x
      pure $ MkCalc (cast x) (cast y) op

    Mult => do
      x <- rndRng32 1 12
      y <- rndRng32 0 (upperBound `div` x)
      pure $ MkCalc (cast x) (cast y) op

randomTile : HasIO io => (Bits8,Bits8) -> io (Bits32, Tile)
randomTile (px,py) = do
  c       <- randomCalc
  sortVal <- rndRng32 0 1000
  pure (sortVal, MkTile px py c)

randomGame : HasIO io => io (List Tile, String)
randomGame = do
  pic   <- rndSelect pictures
  pairs <- traverse randomTile [| MkPair [the Bits8 0..3] [the Bits8 0..3] |]
  let ts = snd <$> sortBy (comparing fst) pairs
  pure $ (ts,pic)

parameters {auto ct : CanvasRenderingContext2D}

  tile : Tile -> IO1 ()
  tile t = fillRect (cast t.posX) (cast t.posY) 1 1

  dispState : MathST -> IO1 ()
  dispState gs = T1.do
    save
    let sf := cast {to = Double} wcanvas / cast gs.rows
    scale sf sf
    fillStyle black
    traverse1_ tile gs.calcs
    fillStyle stuckColor
    traverse1_ tile gs.wrong
    restore

checkAnswer : MathST -> MathST
checkAnswer (MS l input _ nr wrong (h :: t) pic) =
  let ans := cast {to = Integer} input
   in if result h.calc == ans
         then MS l "" (Just Correct) nr wrong t pic
         else MS l "" (Just $ Wrong h.calc ans) nr (h::wrong) t pic
checkAnswer gs = {result := Just Ended} gs

update : HasIO io => MathEv -> MathST -> io MathST
update (Lang x)         s = pure $ {lang := x} s
update Check            s = pure $ checkAnswer s
update (Inp a)          s = pure $ {answer := a} s
update MathInit         s = do
  (ts,pic) <- randomGame
  pure $
    { answer := ""
    , result := Nothing
    , wrong  := []
    , calcs  := ts
    , pic    := pic
    } s

export
run : Prog Void ()
run =
  mvcAct MathInit MathGame.init $ \e,s => Prelude.do
    s2 <- update e s
    case e of
      Lang x   => child exampleDiv (content x)
      Check    => setValue resultIn ""
      MathInit => child exampleDiv (content s2.lang)
      Inp _    => pure ()
    disabledM checkBtn $ currentCalc s2
    disabledM resultIn $ currentCalc s2
    text calc $ maybe "" dispCalc (currentCalc s2)
    text out  $ maybe "" (reply s2.lang) s2.result
    attr pic  $ style "background-image : url('\{s.pic}');"
    attr out  $ style (fromMaybe "" $ s2.result >>= style)
    render pic (dispState s2)
    pure s2
