module Web.Canvas.Transformation

import Data.Linear.Token
import Web.Canvas.Angle
import Web.Canvas.Hints
import Web.Internal.HtmlPrim

parameters {auto ct : CanvasTransform}

  export %inline
  rotate : Angle -> IO1 ()
  rotate a = ffi $ prim__rotate ct a.radians

  export %inline
  scale : (x,y : Double) -> IO1 ()
  scale x y = ffi $ prim__scale ct x y

  export %inline
  translate : (x,y : Double) -> IO1 ()
  translate x y = ffi $ prim__translate ct x y

  export
  transform : (a,b,c,d,e,f : Double) -> IO1 ()
  transform a b c d e f = ffi $ prim__setTransform ct a b c d e f
