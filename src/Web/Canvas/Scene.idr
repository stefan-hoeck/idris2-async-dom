module Web.Canvas.Scene

import Data.Linear.Token
import Web.Canvas.Hints
import Web.Canvas.Shape
import Web.Canvas.Style
import Web.Canvas.Transformation
import Web.Internal.HtmlPrim

%default total

--------------------------------------------------------------------------------
--          Text Metrics
--------------------------------------------------------------------------------

export
%foreign "browser:lambda:x=>x.actualBoundingBoxAscent"
actualBoundingBoxAscent : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.actualBoundingBoxDescent"
actualBoundingBoxDescent : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.actualBoundingBoxLeft"
actualBoundingBoxLeft : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.actualBoundingBoxRight"
actualBoundingBoxRight : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.alphabeticBaseline"
alphabeticBaseline : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.emHeightAscent"
emHeightAscent : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.emHeightDescent"
emHeightDescent : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.fontBoundingBoxAscent"
fontBoundingBoxAscent : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.fontBoundingBoxDescent"
fontBoundingBoxDescent : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.hangingBaseline"
hangingBaseline : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.ideographicBaseline"
ideographicBaseline : TextMetrics -> Double

export
%foreign "browser:lambda:x=>x.width"
width : TextMetrics -> Double

%foreign "browser:lambda:(c,d,a,b,f,s,w)=>{d0 = c.direction; b0 = c.textBaseline; a0 = c.textAlign; f0 = c.font; c.font = f; c.direction = d; c.textBaseline = b; c.textAlign = a; res = c.measureText(s); c.font = f0; c.direction = d0; c.textBaseline = b0; c.textAlign = a0; return res}"
prim__measureText :
     CanvasText
  -> (dir, align, baseline, font, text : String)
  -> PrimIO TextMetrics

||| Compute the `TextMetrics` for the given text in the given font.
export %inline
measureText :
     {auto ct : CanvasText}
  -> CanvasDirection
  -> CanvasTextAlign
  -> CanvasTextBaseline
  -> (font,text : String)
  -> IO1 TextMetrics
measureText d a b f t = ffi $ prim__measureText ct (toFFI d) (toFFI a) (toFFI b) f t

parameters {auto cs : CanvasState}
  export %inline
  save : IO1 ()
  save = ffi $ prim__save cs

  export %inline
  restore : IO1 ()
  restore = ffi $ prim__restore cs
