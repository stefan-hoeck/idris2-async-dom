module Web.Canvas.Shape

import Data.Linear.Token
import Web.Canvas.Angle
import Web.Canvas.Hints
import Web.Internal.HtmlPrim

%default total

parameters {auto cr : CanvasRect}
  export %inline
  clearRect : (x,y,width,height : Double) -> IO1 ()
  clearRect x y width height = ffi $ prim__clearRect cr x y width height

  export %inline
  fillRect : (x,y,width,height : Double) -> IO1 ()
  fillRect x y width height = ffi $ prim__fillRect cr x y width height

  export %inline
  strokeRect : (x,y,width,height : Double) -> IO1 ()
  strokeRect x y width height = ffi $ prim__strokeRect cr x y width height

parameters {auto cp : CanvasPath}

  export %inline
  arc :
       (x,y                            : Double)
    -> (radius                         : Double)
    -> (startAngle, endAngle           : Angle)
    -> {default false counterclockwise : Boolean}
    -> IO1 ()
  arc x y r sa ea =
    ffi $ prim__arc cp x y r sa.radians ea.radians (def counterclockwise)

  export %inline
  arcTo : (x1,y1,x2,y2,radius : Double) -> IO1 ()
  arcTo x1 y1 x2 y2 r = ffi $ prim__arcTo cp x1 y1 x2 y2 r

  export
  circle : (x,y,radius : Double) -> IO1 ()
  circle x y r = arc x y r (rad 0) (rad $ 2 * pi)

  export %inline
  bezierCurveTo : (cp1x, cp1y, cp2x, cp2y, x, y : Double) -> IO1 ()
  bezierCurveTo cp1x cp1y cp2x cp2y x y =
    ffi $ prim__bezierCurveTo cp cp1x cp1y cp2x cp2y x y

  export %inline
  closePath : IO1 ()
  closePath = ffi $ prim__closePath cp

  export %inline
  ellipse :
       (x,y                  : Double)
    -> (radiusX, radiusY     : Double)
    -> (rotation             : Angle)
    -> (startAngle, endAngle : Angle)
    -> {default false counterclockwise : Boolean}
    -> IO1 ()
  ellipse x y rx ry rot sa ea =
    ffi $ prim__ellipse cp x y rx ry
      rot.radians sa.radians ea.radians
      (def counterclockwise)

  export %inline
  lineTo : (x,y : Double) -> IO1 ()
  lineTo x y = ffi $ prim__lineTo cp x y

  export %inline
  moveTo : (x,y : Double) -> IO1 ()
  moveTo x y = ffi $ prim__moveTo cp x y

  export %inline
  rect : (x,y,width,height : Double) -> IO1 ()
  rect x y width height = ffi $ prim__rect cp x y width height

  export %inline
  quadraticCurveTo : (cpx, cpy, x, y : Double) -> IO1 ()
  quadraticCurveTo cpx cpy x y =
    ffi $ prim__quadraticCurveTo cp cpx cpy x y

parameters {auto cp : CanvasDrawPath}

  export %inline
  beginPath : IO1 ()
  beginPath = ffi $ prim__beginPath cp

  export %inline
  clip : IO1 ()
  clip = ffi $ prim__clip cp undef

  export %inline
  fill : IO1 ()
  fill = ffi $ prim__fill cp undef

  export %inline
  stroke : IO1 ()
  stroke = ffi $ prim__stroke cp

parameters {auto cs : CanvasText}

  export %inline
  fillText :
       (txt : String)
    -> (x,y : Double)
    -> {default undef maxWidth : UndefOr Double}
    -> IO1 ()
  fillText txt x y = ffi $ prim__fillText cs txt x y maxWidth

  export %inline
  strokeText :
       (txt : String)
    -> (x,y : Double)
    -> {default undef maxWidth : UndefOr Double}
    -> IO1 ()
  strokeText txt x y = ffi $ prim__strokeText cs txt x y maxWidth
