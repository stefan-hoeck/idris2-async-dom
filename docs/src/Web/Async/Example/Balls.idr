module Web.Async.Example.Balls

import Data.Either
import Data.Linear.Traverse1
import Data.Nat
import Data.Refined.Integer
import Data.Vect

import Derive.Literal
import Derive.Prelude

import Web.Async.Animate
import Web.Async.Example.CSS.Colors
import Web.Async.Example.CSS.Balls
import Web.Async.Example.Util

import Syntax.T1
import Text.CSS.Color
import Web.Canvas

%default total
%language ElabReflection

-- 2D Vector
V2 : Type
V2 = Vect 2 Double

-- Velocity of a point in 2D space
Velocity : Type
Velocity = V2

-- Acceleration of a point in 2D space
Acceleration : Type
Acceleration = V2

-- constant acceleration vector
acc : Acceleration
acc = [0,-9.81]

-- height and width of the room in m
w : Double
w = 10

-- start height of all balls
h0 : Double
h0 = 9

-- ball radius in m
r : Double
r = 0.1

-- start velocity in m/s
v0 : Double
v0 = 4

-- vector addition
(+) : V2 -> V2 -> V2
[u,v] + [x,y] = [u+x, v+y]

-- multiplication with a scalar
(*) : Double -> V2 -> V2
m * [x,y] = [m * x, m * y]

record Ball where
  constructor MkBall
  col : Color
  pos : V2
  vel : Velocity

MinBalls, MaxBalls : Integer
MinBalls  = 1
MaxBalls  = 20000

record NumBalls where
  constructor B
  value : Integer
  {auto 0 prf : FromTo MinBalls MaxBalls value}

%runElab derive "NumBalls" [Show,Eq,Ord,IntegerLit]

data BallsEv : Type where
  Run        : BallsEv
  NumIn      : Either String NumBalls -> BallsEv
  Next       : DTime -> BallsEv

record BallsST where
  constructor BS
  balls    : List Ball
  count    : Nat
  dtime    : DTime
  numBalls : Maybe NumBalls

fpsCount : Nat
fpsCount = 15

init : BallsST
init = BS [] fpsCount 0 Nothing

read : String -> Either String NumBalls
read =
  let err := "expected integer between \{show MinBalls} and \{show MaxBalls}"
   in maybeToEither err . refineNumBalls . cast

inBounds : Ball -> Bool
inBounds (MkBall _ [x,y] _) = y >= 0 && x >= 0 && x <= w

-- room wall thickness in meters
wallThickness : Double
wallThickness = 0.20

parameters {auto ct : CanvasRenderingContext2D}
  ball : Ball -> IO1 ()
  ball b@(MkBall _ [x,y] _) = T1.do
    fillStyle (if inBounds b then b.col else transparent)
    beginPath
    circle x (w-y) r
    fill

  -- walls and floor of the room.
  walls : IO1 ()
  walls = T1.do
    beginPath
    let hwt := wallThickness / 2
    moveTo (-hwt)  0
    lineTo (-hwt)  (w+hwt)
    lineTo (w+hwt) (w+hwt)
    lineTo (w+hwt) 0
    stroke

  balls : List Ball -> IO1 ()
  balls bs = T1.do
    save
    transform 50 0 0 50 10 10
    traverse1_ ball bs
    strokeStyle base80
    lineWidth wallThickness
    walls
    restore

-- canvas width and height
wcanvas : Bits32
wcanvas = 520

content : Sink BallsEv => HTMLNode
content =
  div [ class ballsContent ]
    [ lbl "Number of balls:" lblCount
    , input
        [ Id txtCount
        , onInput (NumIn . read)
        , onEnterDown Run
        , class widget
        , placeholder "Range: [\{show MinBalls}, \{show MaxBalls}]"
        ]
    , button [Id btnRun, onClick Run, disabled True, classes [widget,btn]] ["Run"]
    , div [Id log] []
    , canvas [Id out, width wcanvas, height wcanvas] []
    ]

-- Collision detection: We verify that the given ball
-- is still in the room. If this is not the case, we simulate
-- a bouncing off the walls by inverting the x-velocity (if the
-- ball hit a wall) or the y-velocity (if the ball hit the ground)
checkBounds : Ball -> Ball
checkBounds b@(MkBall c [px,py] [vx,vy]) =
  if      (py <= r  && vy < 0)      then (MkBall c [px,py] [vx,-vy])
  else if (px <= r  && vx < 0)      then (MkBall c [px,py] [-vx,vy])
  else if (px >= (w - r) && vx > 0) then (MkBall c [px,py] [-vx,vy])
  else b

-- moves a ball after a given time delta
-- by adjusting its position and velocity
nextBall : DTime -> Ball -> Ball
nextBall delta (MkBall c p v) =
  let dt := cast delta / the Double 1000 -- time in seconds
      v2 := v + (dt * acc)
      p2 := p + (dt / 2 * (v + v2))
   in checkBounds (MkBall c p2 v2)

initialBalls : NumBalls -> List Ball
initialBalls (B n) = go (cast n) Nil

  where
    col : Bits8 -> Color
    col 0 = comp100
    col 1 = comp80
    col 2 = comp60
    col 3 = comp40
    col _ = comp20

    ball : Nat -> Ball
    ball k =
      let factor := cast {to = Double} k / (cast n - 1.0)
          phi    := pi * factor
          x0     := 1.0 + factor * 8
       in MkBall (col $ cast k `mod` 5) [x0,9] (v0 * [sin phi, cos phi])

    go : (k : Nat) -> List Ball -> List Ball
    go 0     bs = bs
    go (S k) bs = go k $ ball k :: bs

showFPS : Bits32 -> String
showFPS 0 = ""
showFPS n =
  let val := 1000 * cast fpsCount `div` n
   in "FPS: \{show val}"

next : DTime -> BallsST -> BallsST
next m s =
  case s.count of
    0   => { balls $= map (nextBall m), dtime := 0, count := fpsCount } s
    S k => { balls $= map (nextBall m), dtime $= (+m), count := k } s

export
run : JSStream Void
run = do
  E s <- exec $ eventFrom (Next 0)
  exec $ child exampleDiv content
  bracket (lift1 $ animate Next) lift1 $ \_ =>
    mvcActEvs s {s = BallsST} init $ \e,s => do
      case e of
        Run       => pure $ {balls := maybe s.balls initialBalls s.numBalls} s
        NumIn x   => do
          validate txtCount x
          disabledE btnRun x
          pure $ {numBalls := eitherToMaybe x} s
        Next m  => do
          let s2 := next m s
          render out (balls s2.balls)
          when (s2.count == 0) (text log $ showFPS s2.dtime)
          pure s2
