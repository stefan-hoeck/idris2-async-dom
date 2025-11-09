module Web.Canvas.Style

import Data.Linear.Token
import Text.CSS.Color
import Web.Canvas.Hints
import Web.Internal.HtmlPrim

%default total

parameters {auto ct : CanvasTextDrawingStyles}

  export %inline
  font : String -> IO1 ()
  font s = ffi $ prim__setFont ct s

  export %inline
  direction : CanvasDirection -> IO1 ()
  direction d = ffi $ prim__setDirection ct (toFFI d)

  export %inline
  textAlign : CanvasTextAlign -> IO1 ()
  textAlign a = ffi $ prim__setTextAlign ct (toFFI a)

  export %inline
  textBaseline : CanvasTextBaseline -> IO1 ()
  textBaseline b = ffi $ prim__setTextBaseline ct (toFFI b)

parameters {auto cp : CanvasPathDrawingStyles}

  export %inline
  lineWidth : Double -> IO1 ()
  lineWidth d = ffi $ prim__setLineWidth cp d

  export %inline
  lineDashOffset : Double -> IO1 ()
  lineDashOffset d = ffi $ prim__setLineDashOffset cp d

parameters {auto cf : CanvasFillStrokeStyles}

  export %inline
  fillStyle : Color -> IO1 ()
  fillStyle c = ffi $ prim__setFillStyle cf (toUnion3 $ inject $ interpolate c)

  export %inline
  strokeStyle : Color -> IO1 ()
  strokeStyle c = ffi $ prim__setStrokeStyle cf (toUnion3 $ inject $ interpolate c)
