module Web.Async.Event

import JS
import Text.HTML.Event
import Syntax.T1
import Web.Internal.DomPrim
import Web.Internal.GeometryPrim
import Web.Internal.Types
import Web.Internal.UIEventsPrim

%default total

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

%foreign "browser:lambda:x=>x.target.value || x.target.innerHTML || ''"
prim__input : Event -> String

%foreign "browser:lambda:x=>x.target.checked?1:0"
prim__checked : Event -> Bool

%foreign "browser:lambda:x=>x.target.files || []"
prim__files : Event -> FileList

%foreign "browser:lambda:x=>x.length"
prim__length : FileList -> Bits32

%foreign "browser:lambda:(x,y)=>x[y]"
prim__item : FileList -> Bits32 -> File

files : Event -> List File
files e =
 let fs := prim__files e
  in case prim__length fs of
       0 => []
       x => prim__item fs <$> [0..x-1]

--------------------------------------------------------------------------------
-- Event Readers
--------------------------------------------------------------------------------

export
toRect : DOMRect -> IO1 Rect
toRect r = T1.do
  x <- ffi $ DOMRectReadOnly.prim__x (up r)
  y <- ffi $ DOMRectReadOnly.prim__y (up r)
  h <- ffi $ DOMRectReadOnly.prim__height (up r)
  w <- ffi $ DOMRectReadOnly.prim__width (up r)
  t <- ffi $ DOMRectReadOnly.prim__top (up r)
  b <- ffi $ DOMRectReadOnly.prim__bottom (up r)
  l <- ffi $ DOMRectReadOnly.prim__left (up r)
  r <- ffi $ DOMRectReadOnly.prim__right (up r)
  pure (MkRect x y h w t b l r)

%inline
bool : PrimIO Boolean -> IO1 Bool
bool act t = let b # t := ffi act t in eqv b true # t

export
mouseInfo : MouseEvent -> IO1 MouseInfo
mouseInfo e = T1.do
  b  <- ffi $ prim__button e
  bs <- ffi $ prim__buttons e
  cx <- ffi $ prim__clientX e
  cy <- ffi $ prim__clientY e
  ox <- ffi $ prim__offsetX e
  oy <- ffi $ prim__offsetY e
  px <- ffi $ prim__pageX e
  py <- ffi $ prim__pageY e
  sx <- ffi $ prim__screenX e
  sy <- ffi $ prim__screenY e
  a  <- bool $ prim__altKey e
  c  <- bool $ prim__ctrlKey e
  m  <- bool $ prim__metaKey e
  s  <- bool $ prim__shiftKey e
  pure (MkMouseInfo b bs cx cy ox oy px py sx sy a c m s)

export
keyInfo : KeyboardEvent -> IO1 KeyInfo
keyInfo e = T1.do
  k  <- ffi $ prim__key e
  cd <- ffi $ prim__code e
  l  <- ffi $ prim__location e
  ic <- bool $ prim__isComposing e
  a  <- bool $ prim__altKey e
  c  <- bool $ prim__ctrlKey e
  m  <- bool $ prim__metaKey e
  s  <- bool $ prim__shiftKey e
  pure (MkKeyInfo k cd l ic a c m s)

export
changeInfo : Event -> IO1 InputInfo
changeInfo e t =
  MkInputInfo (prim__input e) (files e) (prim__checked e) # t

export %inline
inputInfo : InputEvent -> IO1 InputInfo
inputInfo e = changeInfo $ up e

export
elemScrollInfo : Element -> IO1 ScrollInfo
elemScrollInfo x = T1.do
  st <- ffi $ prim__scrollTop x
  sh <- ffi $ prim__scrollHeight x
  ch <- ffi $ prim__clientHeight x
  pure (MkScrollInfo st sh ch)

export
scrollInfo : Event -> IO1 ScrollInfo
scrollInfo e = T1.do
  net <- ffi $ prim__target e
  case nullableToMaybe net >>= castTo Element of
    Nothing => pure $ MkScrollInfo 0 0 0
    Just t  => elemScrollInfo t

export
wheelInfo : WheelEvent -> IO1 WheelInfo
wheelInfo e = T1.do
  dm <- ffi $ prim__deltaMode e
  dx <- ffi $ prim__deltaX e
  dy <- ffi $ prim__deltaY e
  dz <- ffi $ prim__deltaZ e
  pure (MkWheelInfo dm dx dy dz)
