module Web.Async.View

import Data.Linear.Traverse1
import Data.Either
import Data.Maybe
import Data.String
import IO.Async.Logging
import JS
import Syntax.T1
import Text.CSS
import Text.HTML
import Text.HTML.DomID
import Web.Async.Event
import Web.Async.Util
import Web.Internal.Types

%hide Data.Linear.(.)
%default total

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

%foreign "browser:lambda:(e,f,w) => {const o = new ResizeObserver((es) => f(e.getBoundingClientRect())(w));o.observe(e)}"
prim__observeResize : Element -> (DOMRect -> PrimIO ()) -> PrimIO ()

export
%foreign "browser:lambda:x=>x.bubbles?1:0"
bubbles : Event -> Bool

export
%foreign "browser:lambda:x=>x.cancelable?1:0"
cancelable : Event -> Bool

%foreign "browser:lambda:(x,s,f,w)=>x.addEventListener(s,\e => f(e)(w))"
prim__addlistener : EventTarget -> String -> (Event -> PrimIO ()) -> PrimIO ()

%foreign "browser:lambda:(x,w)=>x.preventDefault()"
prim__preventDefault : Event -> PrimIO ()

%foreign "browser:lambda:(x,w)=>x.stopPropagation()"
prim__stopPropagation : Event -> PrimIO ()

--------------------------------------------------------------------------------
-- IO1
--------------------------------------------------------------------------------

export %inline
addEventListener : EventTarget -> String -> (Event -> IO1 ()) -> IO1 ()
addEventListener et ev cb = ffi $ prim__addlistener et ev (primRun . cb)

export %inline
preventDefault : Event -> IO1 ()
preventDefault ev = ffi $ prim__preventDefault ev

export %inline
stopPropagation : Event -> IO1 ()
stopPropagation ev = ffi $ prim__stopPropagation ev

--------------------------------------------------------------------------------
-- Event Handler
--------------------------------------------------------------------------------

||| Low level method for registering `DOMEvents` at
||| HTML elements.
|||
||| Use this, for instance, to register `DOMEvents` at
||| a HTMLElement of a static document.
export
registerDOMEvent :
     {auto h : Sink e}
  -> (preventDefault, stopPropagation : Bool)
  -> EventTarget
  -> DOMEvent e
  -> IO1 ()
registerDOMEvent prev stop el de =
  case de of
    Input f      => inst "input" changeInfo f
    Change f     => inst "change" changeInfo f
    Click f      => inst "click" mouseInfo f
    DblClick f   => inst "dblclick" mouseInfo f
    KeyDown f    => inst "keydown" keyInfo f
    KeyUp f      => inst "keyup" keyInfo f
    Blur v       => inst "blur" {t = Event} (const $ pure v) Just
    Focus v      => inst "focus" {t = Event} (const $ pure v) Just
    MouseDown f  => inst "mousedown" mouseInfo f
    MouseUp f    => inst "mouseup" mouseInfo f
    MouseEnter f => inst "mouseenter" mouseInfo f
    MouseLeave f => inst "mouseleave" mouseInfo f
    MouseOver f  => inst "mouseover" mouseInfo f
    MouseOut f   => inst "mouseout" mouseInfo f
    MouseMove f  => inst "mousemove" mouseInfo f
    HashChange v => inst "hashchange" {t = Event} (const $ pure v) Just
    Scroll f     => inst "scroll" scrollInfo f
    Wheel f      => inst "wheel" wheelInfo f
    Resize f     => onresize f

  where
    inst :
         {0 t,b : _}
      -> {auto c : SafeCast t}
      -> String
      -> (t -> IO1 b)
      -> (b -> Maybe e)
      -> IO1 ()
    inst {t} s conv f =
     let cb : Event -> IO1 ()
         cb e = T1.do
           when1 (cancelable e && prev) (preventDefault e)
           when1 (bubbles e && stop) (stopPropagation e)
           let Just vt := castTo t e | Nothing => pure ()
           vb <- conv vt
           maybe (pure ()) h.sink1 (f vb)

      in addEventListener el s cb

    onresize : (Rect -> Maybe e) -> IO1 ()
    onresize f =
      let Just va := castTo Element el | Nothing => pure ()
       in ffi $ prim__observeResize va $ \r => primRun $
            toRect r >>= maybe (pure ()) h.sink1 . f

export
setAttribute : Element -> Attribute t -> IO1 ()
setAttribute el (Id (Id v))       = ffi $ prim__setAttribute el "id" v
setAttribute el (Str name v)      = ffi $ prim__setAttribute el name v
setAttribute el (Bool name True)  = ffi $ prim__setAttribute el name ""
setAttribute el (Bool name False) = ffi $ prim__removeAttribute el name
setAttribute el (Event_ p s ev)   = registerDOMEvent p s (up el) ev
setAttribute el Empty             = pure ()

--------------------------------------------------------------------------------
-- Node Preparation
--------------------------------------------------------------------------------

addNodes : ParentNode -> HTMLNodes -> IO1 ()

addNode : ParentNode -> HTMLNode -> IO1 ()
addNode p (El {tag} _ xs ys) t =
 let n # t := ffi (prim__createElement tag) t
     _ # t := ffi (prim__append p (up n)) t
     _ # t := addNodes (up n) ys t
  in traverse1_ (setAttribute n) xs t
addNode p (EEl {tag} _ xs) t =
 let n # t := ffi (prim__createElement tag) t
     _ # t := ffi (prim__append p (up n)) t
  in traverse1_ (setAttribute n) xs t

addNode p (Raw s) t =
 let el # t := ffi (prim__createElement "template") t
     Just temp := castTo HTMLTemplateElement el | Nothing => () # t
     _  # t := ffi (prim__setInnerHTML (up temp) s) t
     c  # t := ffi (prim__content temp) t
  in ffi (prim__append p (up c)) t

addNode p (Text s) t = ffi (prim__appendTxt p s) t

addNode p Empty      t = () # t

addNodes p = assert_total $ traverse1_ (addNode p)

parameters {auto has : Has JSErr es}

  %inline
  setupNodes : (Element -> Node -> JS es ()) -> Ref t -> HTMLNodes -> JS es ()
  setupNodes adj r ns = do
    elem <- castElementByRef {t = Element} r
    df   <- primIO prim__createDocumentFragment
    lift1 $ addNodes (up df) ns
    adj elem (up df)

  %inline
  setupNode :
       (Element -> Node -> JS es ())
    -> Ref t
    -> HTMLNode
    -> JS es ()
  setupNode adj r n = setupNodes adj r [n]

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| inserts them as the children of the given target.
  export
  children : Ref t -> HTMLNodes -> JS es ()
  children = setupNodes (\el => replaceChildren (up el))

  ||| Sets up the reactive behavior of the given `Node` and
  ||| inserts it as the only child of the given target.
  export
  child : Ref t -> HTMLNode -> JS es ()
  child = setupNode (\el => replaceChildren (up el))

  ||| Replaces the given node's children with a text node
  ||| displaying the given string.
  export %inline
  text : Ref t -> String -> JS es ()
  text r = child r . Text

  ||| Replaces the given node's children with a text node
  ||| showing the given value.
  export %inline
  show : Show b => Ref t -> b -> JS es ()
  show r = text r . show

  ||| Replaces the given node's children with the raw
  ||| HTML passed as a string argument.
  export %inline
  raw : Ref t -> String -> JS es ()
  raw r = child r . Raw

  ||| Replaces the given `<style>` node's CSS rules.
  export
  style : Ref Tag.Style -> List (Rule 1) -> JS es ()
  style r = raw r . fastUnlines . map interpolate

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| inserts them after the given child node.
  export
  afterMany : Ref t -> HTMLNodes -> JS es ()
  afterMany = setupNodes (\el => after (up el))

  ||| Sets up the reactive behavior of the given `Node` and
  ||| inserts it after the given child node.
  export
  after : Ref t -> HTMLNode -> JS es ()
  after = setupNode (\el => after (up el))

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| inserts them before the given child node.
  export
  beforeMany : Ref t -> HTMLNodes -> JS es ()
  beforeMany = setupNodes (\el => before (up el))

  ||| Sets up the reactive behavior of the given `Node` and
  ||| inserts it before the given child node.
  export
  before : Ref t -> HTMLNode -> JS es ()
  before = setupNode (\el => before (up el))

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| appends them to the given element's list of children
  export
  appendMany : Ref t -> HTMLNodes -> JS es ()
  appendMany = setupNodes (\el => append (up el))

  ||| Sets up the reactive behavior of the given `Node` and
  ||| appends it to the given element's list of children
  export
  append : Ref t -> HTMLNode -> JS es ()
  append = setupNode (\el => append (up el))

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| prepends them to the given element's list of children
  export
  prependMany : Ref t -> HTMLNodes -> JS es ()
  prependMany = setupNodes (\el => prepend (up el))

  ||| Sets up the reactive behavior of the given `Node` and
  ||| prepends it to the given element's list of children
  export
  prepend : Ref t -> HTMLNode -> JS es ()
  prepend = setupNode (\el => prepend (up el))

  ||| Sets up the reactive behavior of the given `Node`s and
  ||| replaces the given element.
  export
  replaceMany : Ref t -> HTMLNodes -> JS es ()
  replaceMany = setupNodes (\el => replace (up el))

  ||| Sets up the reactive behavior of the given `Node` and
  ||| replaces the given element.
  export
  replace : Ref t -> HTMLNode -> JS es ()
  replace = setupNode (\el => replace (up el))

  ||| Removes the given element from the DOM.
  export
  remove : Ref t -> JS es ()
  remove r =
    castElementByRef {t = Element} r >>= \el => primIO (prim__remove (up el))

  ||| Sets an attribute at the given node.
  export
  attr : Ref t -> Attribute t -> JS es ()
  attr r a = castElementByRef r >>= \el => lift1 $ setAttribute el a


  ||| Sets the `checked` property of the given element
  export
  checked : Ref Tag.Input -> Bool -> JS es ()
  checked r b =
    castElementByRef r >>= \el => primIO (prim__setChecked el $ toFFI b)

  ||| Sets the `disabled` attribute of the given element
  export %inline
  disabled : Ref t -> Bool -> JS es ()
  disabled r = attr r . disabled

  ||| Sets the `disabled` attribute of the given element
  ||| if the given values is a `Left`.
  |||
  ||| This is useful for disabling components such as buttons
  ||| in the UI in case of invalid user input.
  export %inline
  disabledE : {0 a,b : _} -> Ref t -> Either a b -> JS es ()
  disabledE r = disabled r . isLeft

  ||| Sets the `disabled` attribute of the given element
  ||| if the given values is a `Nothing`.
  |||
  ||| This is useful for disabling components such as buttons
  ||| in the UI in case of invalid user input.
  export %inline
  disabledM : {0 a : _} -> Ref t -> Maybe a -> JS es ()
  disabledM r = disabled r . isNothing

  ||| Focus the given HTMLElemet
  export %inline
  focus : Ref t -> JS es ()
  focus r = castElementByRef {t = HTMLElement} r >>= focus

  ||| Blur (lose focus on) the given HTMLElemet
  export %inline
  blur : Ref t -> JS es ()
  blur r = castElementByRef {t = HTMLElement} r >>= blur

--------------------------------------------------------------------------------
-- Utils with error handling
--------------------------------------------------------------------------------

parameters {auto lg  : Loggable JSErr}
           {auto log : Logger JS}

  export %inline
  elemChildren : DomID -> HTMLNodes -> JS [] ()
  elemChildren i = logErrs . children {es = [JSErr]} (elemRef i)

  export %inline
  elemChild : DomID -> HTMLNode -> JS [] ()
  elemChild i = logErrs . child {es = [JSErr]} (elemRef i)

  export %inline
  elemAppend : DomID -> HTMLNode -> JS [] ()
  elemAppend i = logErrs . append {es = [JSErr]} (elemRef i)

  export %inline
  elemPrepend : DomID -> HTMLNode -> JS [] ()
  elemPrepend i = logErrs . prepend {es = [JSErr]} (elemRef i)

  export %inline
  clearElem : DomID -> JS [] ()
  clearElem i = elemChildren i []

  export %inline
  removeElem : DomID -> JS [] ()
  removeElem = logErrs . remove {es = [JSErr]} . elemRef

  export %inline
  replaceElem : DomID -> HTMLNode -> JS [] ()
  replaceElem i = logErrs . replace {es = [JSErr]} (elemRef i)

  export %inline
  btnAttr : DomID -> Attribute Tag.Button -> JS [] ()
  btnAttr v a = logErrs $ attr {es = [JSErr]} (btnRef v) a
