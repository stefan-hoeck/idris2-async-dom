module Web.Async.Util

import Web.Internal.Types
import public FS
import public FS.Concurrent.Signal
import public IO.Async.JS
import public JS
import public Text.HTML

%default total

--------------------------------------------------------------------------------
--          FFI
--------------------------------------------------------------------------------

%foreign "browser:lambda:(w)=>window"
prim__window : PrimIO Window

%foreign "browser:lambda:(w)=>document"
prim__document : PrimIO Document

%foreign "browser:lambda:(w)=>document.body"
prim__body : PrimIO (Nullable HTMLElement)

%foreign "browser:lambda:(x,w)=>document.getElementById(x)"
prim__getElementById : String -> PrimIO (Nullable Element)

%foreign "browser:lambda:(x,s,w)=>x.setCustomValidity(s)"
prim__setCustomValidity : Element -> String -> PrimIO ()

%foreign "browser:lambda:(x,s,w)=> {x.value = s;}"
prim__setValue : Element -> String -> PrimIO ()

%foreign "browser:lambda:(x,n,w)=>x.replaceChildren(n)"
prim__replaceChildren : ParentNode -> Node -> PrimIO ()

export
%foreign "browser:lambda:(x,n,w)=>x.append(n)"
prim__append : ParentNode -> Node -> PrimIO ()

export
%foreign "browser:lambda:(x,n,w)=>x.append(n)"
prim__appendTxt : ParentNode -> String -> PrimIO ()

%foreign "browser:lambda:(x,n,w)=>x.prepend(n)"
prim__prepend : ParentNode -> Node -> PrimIO ()

%foreign "browser:lambda:(x,n,w)=>x.after(n)"
prim__after : ChildNode -> Node -> PrimIO ()

%foreign "browser:lambda:(x,n,w)=>x.before(n)"
prim__before : ChildNode -> Node -> PrimIO ()

%foreign "browser:lambda:(x,n,w)=>x.replaceWith(n)"
prim__replace : ChildNode -> Node -> PrimIO ()

export
%foreign "browser:lambda:(x,w)=>x.remove()"
prim__remove : ChildNode -> PrimIO ()

export
%foreign "browser:lambda:(s,w)=>document.createElement(s)"
prim__createElement : String -> PrimIO Element

export
%foreign "browser:lambda:(x,a,b,w)=>x.setAttribute(a,b)"
prim__setAttribute : Element -> String -> String -> PrimIO ()

export
%foreign "browser:lambda:(x,a,w)=>x.removeAttribute(a)"
prim__removeAttribute : Element -> String -> PrimIO ()

export
%foreign "browser:lambda:(x,w)=>x.innerHTML"
prim__innerHTML : InnerHTML -> PrimIO String

export
%foreign "browser:lambda:(x,v,w)=>{x.innerHTML = v}"
prim__setInnerHTML : InnerHTML -> String -> PrimIO ()

export
%foreign "browser:lambda:(x,w)=>x.content"
prim__content : HTMLTemplateElement -> PrimIO DocumentFragment

export
%foreign "browser:lambda:w=>document.createDocumentFragment()"
prim__createDocumentFragment : PrimIO DocumentFragment

export
%foreign "browser:lambda:(x,v,w)=>{x.checked = v}"
prim__setChecked : HTMLInputElement -> Boolean -> PrimIO ()

export
%foreign "browser:lambda:(x,v,w)=>x.checked?1:0"
prim__checked : HTMLInputElement -> PrimIO Bool

%foreign "browser:lambda:(x,w)=>x.blur()"
prim__blur : HTMLOrSVGElement -> PrimIO ()

%foreign "browser:lambda:(x,w)=>x.focus()"
prim__focus : HTMLOrSVGElement -> PrimIO ()

-- TODO: This should go to the js library
%foreign "javascript:lambda:(w) => BigInt(new Date().getTime())"
prim__time : PrimIO Integer

%foreign "browser:lambda:(s,w) => navigator.clipboard.writeText(s)"
prim__writeToClipboard : String -> PrimIO ()

%foreign "browser:lambda:(f,w) => navigator.clipboard.readText().then(s => f(s)(w))"
prim__readFromClipboard : (String -> PrimIO ()) -> PrimIO ()

--------------------------------------------------------------------------------
--          Core Utilities
--------------------------------------------------------------------------------

||| Return the current window.
export %inline
window : HasIO io => io Window
window = primIO prim__window

||| Return the current document.
export %inline
document : HasIO io => io Document
document = primIO prim__document

||| Return the body element of the current document.
|||
||| Return `Nothing` in case the current document has no body defined.
export
getBody : HasIO io => io (Maybe HTMLElement)
getBody = nullableToMaybe <$> primIO prim__body

||| Replace all children of the given node with a new node.
export %inline
replaceChildren : HasIO io => ParentNode -> Node -> io ()
replaceChildren el n = primIO $ prim__replaceChildren el n

||| Append the given node to a DOM element's children
export %inline
append : HasIO io => ParentNode -> Node -> io ()
append el n = primIO $ prim__append el n

||| Prepend the given node to a DOM element's children
export %inline
prepend : HasIO io => ParentNode -> Node -> io ()
prepend el n = primIO $ prim__prepend el n

||| Insert the given node after a DOM element
export %inline
after : HasIO io => ChildNode -> Node -> io ()
after el n = primIO $ prim__after el n

||| Insert the given node before a DOM element
export %inline
before : HasIO io => ChildNode -> Node -> io ()
before el n = primIO $ prim__before el n

||| Replace a DOM element with the given node.
export %inline
replace : HasIO io => ChildNode -> Node -> io ()
replace el n = primIO $ prim__replace el n

||| Replace a DOM element with the given document fragment.
export %inline
remove : HasIO io => ChildNode -> io ()
remove el = primIO $ prim__remove el

||| Creates a DOM element of the given type.
export %inline
createElement : HasIO io => String -> io Element
createElement s = primIO $ prim__createElement s

||| Sets an attribute of a DOM element.
export %inline
setAttribute : HasIO io => Element -> (name, value : String) -> io ()
setAttribute el n v = primIO $ prim__setAttribute el n v

||| Unsets an attribute of a DOM element.
export %inline
removeAttribute : HasIO io => Element -> (name : String) -> io ()
removeAttribute el n = primIO $ prim__removeAttribute el n

||| Returns the inner HTML structure of a node as a String
export %inline
innerHTML : HasIO io => InnerHTML -> io String
innerHTML n = primIO $ prim__innerHTML n

||| Sets the inner HTML structure of a node.
export %inline
setInnerHTML : HasIO io => InnerHTML -> String -> io ()
setInnerHTML n s = primIO $ prim__setInnerHTML n s

||| Focus the given HTML element
export %inline
focus : HasIO io => HTMLElement -> io ()
focus el = primIO (prim__focus $ up el)

||| Blur (lose focus of) the given HTML element
export %inline
blur : HasIO io => HTMLElement -> io ()
blur el = primIO (prim__blur $ up el)

||| Get the current time in milliseconds since 1970/01/01.
export
currentTime : HasIO io => io Integer
currentTime = primIO prim__time

||| Determine the time taken to run an `IO` action.
export
timed : HasIO io => io t -> io (t,Integer)
timed act = do
  t1 <- currentTime
  v  <- act
  t2 <- currentTime
  pure (v, t2 - t1)

export %inline
timed' : HasIO io => io () -> io Integer
timed' = map snd . timed

||| Writes as string to the clipboard.
export %inline
toClipboard : HasIO io => String -> io ()
toClipboard s = primIO (prim__writeToClipboard s)

export
readFromClipboard1 : HasIO io => (String -> IO1 ()) -> io ()
readFromClipboard1 cb = primIO $ prim__readFromClipboard (\s => primRun (cb s))

export
readFromClipboard : Async e es String
readFromClipboard =
  primAsync_ $ \cb =>
    ffi $ prim__readFromClipboard (\s => primRun (cb $ Right s))

--------------------------------------------------------------------------------
--          Type Computations
--------------------------------------------------------------------------------

||| DOM type associacte with an ElemRef
public export
0 ElemType : Ref t -> Type
ElemType (Id _)   = Element
ElemType (Elem _) = Element
ElemType Body     = HTMLElement
ElemType Document = Document
ElemType Window   = Window

public export
0 JS : List Type -> Type -> Type
JS = Async JS

nodeList : DocumentFragment -> List (HSum [Node,String])
nodeList df = [inject $ df :> Node]

--------------------------------------------------------------------------------
--          Accessing and Updating Nodes
--------------------------------------------------------------------------------

parameters {auto has : Has JSErr es}

  export %inline
  js : JSIO t -> JS es t
  js = injectIO . runEitherT

  export
  jsCast : SafeCast t => String -> s -> JS es t
  jsCast msg = js . tryCast msg

  export
  unmaybe : Lazy String -> Maybe t -> JS es t
  unmaybe msg = maybe (throw $ Caught msg) pure

  export
  body : JS es HTMLElement
  body = getBody >>= unmaybe "document.body returned `Nothing`"

  ||| Tries to retrieve an element of the given type by looking
  ||| up its ID in the DOM.
  |||
  ||| This will throw a `JSErr` in case the element cannot be found
  ||| or cast to the desired result type.
  export
  getElementById : SafeCast t => Maybe String -> (id : String) -> JS es t
  getElementById mtag id = do
    e <- primIO (prim__getElementById id)
    unmaybe msg $ nullableToMaybe e >>= castTo t
    where
      %inline tag, msg : String
      tag = fromMaybe "element" mtag
      msg = "getElementById: Could not find \{tag} with id \{id}"

  ||| Tries to retrieve a HTMLElement by looking
  |||
  ||| This will throw a `JSErr` in case the element cannot be found
  ||| or cast to the desired result type.
  export %inline
  getHTMLElementById : (tag,id : String) -> JS es HTMLElement
  getHTMLElementById = getElementById . Just

  ||| Tries to retrieve an element of the given type by looking
  ||| up its ID in the DOM.
  |||
  ||| This will throw a `JSErr` in case the element cannot be found
  ||| or cast to the desired result type.
  export
  getElementByRef : (r : Ref t) -> JS es (ElemType r)
  getElementByRef (Id {tag} id) = getElementById (Just tag) id
  getElementByRef (Elem id)     = getElementById Nothing id
  getElementByRef Body          = body
  getElementByRef Document      = document
  getElementByRef Window        = window

  err : String
  err = "Web.Async.getElementByRef"

  ||| Tries to retrieve an element of the given type by looking
  ||| up its ID in the DOM.
  |||
  ||| This will throw a `JSErr` in case the element cannot be found
  ||| or cast to the desired result type.
  export
  castElementByRef : {0 x : k} -> SafeCast t => Ref x -> JS es t
  castElementByRef ref = getElementByRef ref >>= jsCast err

  ||| Sets a custom validity message at the given reference.
  |||
  ||| Pass the empty string to mark the element as valid.
  |||
  ||| This will throw a `JSErr` in case the element cannot be found
  ||| or cast to the desired result type.
  export %inline
  validityMessage : Ref t -> (0 p : ValidityTag t) => String -> JS es ()
  validityMessage r s =
    castElementByRef r >>= \e => primIO (prim__setCustomValidity e s)

  ||| Sets or unsets a custom validity message at the given node.
  export
  validate : Ref t -> (0 p : ValidityTag t) => Either String b -> JS es ()
  validate r (Left s)  = validityMessage r s
  validate r (Right s) = validityMessage r ""

  ||| Sets the value of the element identified by the given ID.
  |||
  ||| This will throw a `JSErr` in case the element cannot be found
  ||| or cast to the desired result type.
  export %inline
  setValue : Ref t -> (0 p : ValueTag t) => String -> JS es ()
  setValue r s = castElementByRef r >>= \e => primIO (prim__setValue e s)

--------------------------------------------------------------------------------
--          DOM Streams
--------------------------------------------------------------------------------

public export
0 Act : Type -> Type
Act = Async JS [JSErr]

public export
0 Prog : Type -> Type -> Type
Prog o r = Pull (Async JS) o [JSErr] r

export %hint
signalSink : (r : SignalRef t) => Sink t
signalSink = S (put1 r)

export covering
pullErr : AsyncStream f es Void -> Async f es ()
pullErr s =
  weakenErrors (pull s) >>= \case
    Error err => fail err
    _         => pure ()

export covering %inline
runJS : JS [JSErr] () -> IO ()
runJS = app . handle [putStrLn . dispErr]

export covering %inline
runProg : Prog Void () -> IO ()
runProg = runJS . pullErr

export
mvcActSignal : (evs : SignalRef e) => s -> (e -> s -> Act s) -> Prog Void ()
mvcActSignal ini act = discrete evs |> P.evalScans1 ini (flip act) |> drain

parameters (ev  : e)
           (ini : s)

  export
  mvcAct : (Sink e => e -> s -> Act s) -> Prog Void ()
  mvcAct act = do
    evs <- signal ev
    mvcActSignal @{evs} ini act

  export
  mvc : (e -> s -> s) -> (Sink e => e -> s -> Act ()) -> Prog Void ()
  mvc upd disp =
    mvcAct (\v,x => let y := upd v x in disp v y $> y)
