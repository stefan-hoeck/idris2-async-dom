module Web.Canvas

import Derive.Prelude
import JS
import Text.HTML
import Web.Async.Util

import public Web.Canvas.Angle
import public Web.Canvas.Hints
import public Web.Canvas.Scene
import public Web.Canvas.Shape
import public Web.Canvas.Style
import public Web.Canvas.Transformation

%default total
%language ElabReflection

%foreign "browser:lambda:(x,w)=>x.height"
prim__height : HTMLCanvasElement -> PrimIO Double

%foreign "browser:lambda:(x,v,w)=>{x.height = v}"
prim__setHeight : HTMLCanvasElement -> Double -> PrimIO ()

%foreign "browser:lambda:(x,w)=>x.width"
prim__width : HTMLCanvasElement -> PrimIO Double

%foreign "browser:lambda:(x,v,w)=>{x.width = v}"
prim__setWidth : HTMLCanvasElement -> Double -> PrimIO ()

%foreign "browser:lambda:(x,w)=>x.getContext('2d')"
prim__context2D : HTMLCanvasElement -> PrimIO (Nullable CanvasRenderingContext2D)

||| Canvas dimensions
public export
record CanvasDims where
  [noHints]
  constructor CD
  cwidth  : Double
  cheight : Double

%runElab derive "CanvasDims" [Show,Eq]

parameters {auto has : Has JSErr es}
  export
  canvasDims : Ref Canvas -> JS es CanvasDims
  canvasDims r = do
    canvas <- castElementByRef {t = HTMLCanvasElement} r
    w <- primIO (prim__width canvas)
    h <- primIO (prim__height canvas)
    pure (CD w h)

  export
  setCanvasDims : Ref Canvas -> CanvasDims -> JS es ()
  setCanvasDims r (CD w h) = do
    canvas <- castElementByRef {t = HTMLCanvasElement} r
    primIO (prim__setWidth canvas w)
    primIO (prim__setHeight canvas h)

  export
  context2D : HTMLCanvasElement -> JS es CanvasRenderingContext2D
  context2D canvas = do
  m <- primIO (prim__context2D canvas)
  case nullableToMaybe m of
    Just v  => pure v
    Nothing => throw $ Caught "Web.Canvas.context2D: No 2D rendering context for canvas"

  ||| Render a scene in a canvas in the DOM.
  export
  render : Ref Canvas -> (CanvasRenderingContext2D => IO1 ()) -> JS es ()
  render ref scene = do
  canvas <- castElementByRef {t = HTMLCanvasElement} ref
  ctxt   <- context2D canvas
  w      <- primIO (prim__width canvas)
  h      <- primIO (prim__height canvas)
  lift1 (clearRect 0 0 w h)
  lift1 scene
