module Web.Async.HTTP

import Data.Linear.Traverse1
import Derive.Prelude
import JSON.Simple
import Syntax.T1
import Web.Async.Util
import Web.Async.View
import Web.Internal.Types

%default total
%language ElabReflection

--------------------------------------------------------------------------------
--          FFI
--------------------------------------------------------------------------------

%foreign "browser:lambda:(x,a,b,w)=>x.append(a,b)"
prim__append : FormData -> String -> String -> PrimIO ()


%foreign "browser:lambda:(x,a,b,w)=>x.append(a,b)"
prim__appendBlob : FormData -> String -> Blob -> PrimIO ()

%foreign "browser:lambda:x=>x.responseText"
prim__responseText : XMLHttpRequest -> PrimIO String

%foreign "browser:lambda:x=>x.status"
prim__status : XMLHttpRequest -> PrimIO Bits16

%foreign "browser:lambda:(x,w)=>x.send()"
prim__send : XMLHttpRequest -> PrimIO ()

%foreign "browser:lambda:(x,s,w)=>x.send(s)"
prim__sendTxt : XMLHttpRequest -> String -> PrimIO ()

%foreign "browser:lambda:(w)=> new FormData()"
prim__newFD : PrimIO FormData

%foreign "browser:lambda:(x,s,w)=>x.send(s)"
prim__sendFD : XMLHttpRequest -> FormData -> PrimIO ()

%foreign "browser:lambda:(w)=> new XMLHttpRequest()"
prim__request : PrimIO XMLHttpRequest

%foreign "browser:lambda:(x,w)=>x.readyState"
prim__readyState : XMLHttpRequest -> PrimIO Bits16

%foreign "browser:lambda:(x,v,w)=>{x.timeout = v}"
prim__setTimeout : XMLHttpRequest -> Bits32 -> PrimIO ()

%foreign "browser:lambda:(x,w)=>x.abort()"
prim__abort : XMLHttpRequest -> PrimIO ()

%foreign "browser:lambda:(x,me,url,w)=>x.open(me,url)"
prim__open : XMLHttpRequest -> String -> String -> PrimIO ()

%foreign "browser:lambda:(x,a,b,w)=>x.setRequestHeader(a,b)"
prim__setRequestHeader : XMLHttpRequest -> (h,v : String) -> PrimIO ()

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

||| HTTP methods currently supported.
public export
data Method = GET | POST

%runElab derive "Method" [Show,Eq,Ord]

||| A HTTP header is just a pair of strings.
public export
0 Header : Type
Header = (String,String)

||| Part in a formdata request.
public export
data Part : Type where
  StringPart : (name, value : String) -> Part
  FilePart   : (name : String) -> (file : File) -> Part

||| Body of a HTTP request.
public export
data RequestBody : Type where
  Empty      : RequestBody
  StringBody : (mimeType : String) -> (content : String) -> RequestBody
  JSONBody   : {0 a : Type} -> ToJSON a => a -> RequestBody
  FormBody   : List Part -> RequestBody

||| HTTP Errors
public export
data HTTPError : Type where
  Timeout      : HTTPError
  NetworkError : HTTPError
  BadStatus    : Bits16 -> HTTPError
  JSONError    : String -> DecodingErr -> HTTPError

||| Type of expected respons.
|||
||| Every constructor takes a function for wrapping a request
||| result of type `Either HTTPError x` into the result type.
public export
data Expect : Type -> Type where
  ExpectJSON   : {0 a : _} -> FromJSON a => (Either HTTPError a -> r) -> Expect r
  ExpectString : (Either HTTPError String -> r) -> Expect r
  ExpectAny    : (Either HTTPError () -> r) -> Expect r

bodyHeaders : RequestBody -> List Header
bodyHeaders Empty            = []
bodyHeaders (StringBody m _) = [("Content-Type", m)]
bodyHeaders (JSONBody x)     = [("Content-Type", "application/json")]
bodyHeaders (FormBody x)     = []

append : FormData -> Part -> IO1 ()
append fd (StringPart name value) = ffi $ prim__append  fd name value
append fd (FilePart name file)    = ffi $ prim__appendBlob fd name (up file)

parameters {0 r    : Type}
           {auto s : Sink r}

  onerror : Expect r -> HTTPError -> Event -> IO1 ()
  onerror (ExpectJSON f)   err _ = s.sink1 (f $ Left err)
  onerror (ExpectString f) err _ = s.sink1 (f $ Left err)
  onerror (ExpectAny f)    err _ = s.sink1 (f $ Left err)

  onsuccess : Expect r -> XMLHttpRequest -> Event -> IO1 ()
  onsuccess (ExpectString f)   x _ = T1.do
    txt <- ffi (prim__responseText x)
    s.sink1 (f $ Right txt)
  onsuccess (ExpectAny f)      x _ = s.sink1 (f $ Right ())
  onsuccess (ExpectJSON {a} f) x _ = T1.do
    txt <- ffi (prim__responseText x)
    s.sink1 (f $ mapFst (JSONError txt) $ decode txt)

  onload : Expect r -> XMLHttpRequest -> Event -> IO1 ()
  onload exp x ev = T1.do
    st   <- ffi (prim__status x)
    case st >= 200 && st < 300 of
      False => onerror exp (BadStatus st) ev
      True  => onsuccess exp x ev

  xsend : RequestBody -> XMLHttpRequest -> IO1 ()
  xsend Empty            x = ffi (prim__send x)
  xsend (StringBody _ s) x = ffi (prim__sendTxt x s)
  xsend (JSONBody d)     x = ffi (prim__sendTxt x $ encode d)
  xsend (FormBody ps)    x = T1.do
    fd <- ffi $ prim__newFD
    traverse1_ (append fd) ps
    ffi $ prim__sendFD x fd

  ||| Sends a HTTP request.
  |||
  ||| Converts the response to an event of type `r`.
  export
  request :
       (method  : Method)
    -> (headers : List Header)
    -> (url     : String)
    -> (body    : RequestBody)
    -> (expect  : Expect r)
    -> (timeout : Maybe Bits32)
    -> IO1 (IO1 ())
  request m headers url body exp tout = T1.do
    -- create new Http request
    x <- ffi $ prim__request

    -- register event listeners
    addEventListener (up x) "error" $ onerror exp NetworkError
    addEventListener (up x) "load" $ onload exp x
    addEventListener (up x) "timeout" $ onerror exp Timeout

    -- open url
    ffi $ prim__open x (show m) url

    -- set message headers
    let hs := bodyHeaders body ++ headers
    for1_ hs $ \(n,h) => ffi $ prim__setRequestHeader x n h

    -- set timeout (if any)
    for1_ tout $ \v => ffi $ prim__setTimeout x v

    xsend body x
    pure (ffi $ prim__abort x)

  ||| Send a GET HTTP request.
  export %inline
  get : (url : String) -> (expect : Expect r) -> IO1 (IO1 ())
  get u e = request GET [] u Empty e Nothing

  ||| Send a GET request, reading the response as plain text.
  export %inline
  getText : (url : String) -> (f : Either HTTPError String -> r) -> IO1 (IO1 ())
  getText u = get u . ExpectString

  ||| Send a GET request, decoding the result as a JSON string
  ||| and converting it to the result type `a`.
  export %inline
  getJSON : FromJSON t => (url : String) -> (f : Either HTTPError t -> r) -> IO1 (IO1 ())
  getJSON u = get u . ExpectJSON

  ||| Send a POST request.
  export %inline
  post : (url : String) -> (body : RequestBody) -> (expect : Expect r) -> IO1 (IO1 ())
  post u b e = request POST [] u b e Nothing
