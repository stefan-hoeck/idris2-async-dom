module Web.Async.Widget

import Data.Linear.Unique
import Data.List1
import Data.String
import Text.CSS.Declaration
import Text.CSS.Property
import Text.HTML
import Text.HTML.DomID
import Text.HTML.Select
import Web.Async.I18n
import Web.Async.Util
import Web.Async.View
import Web.Internal.Types

import public Web.Async.I18n
import public Web.Async.Widget.Types

%default total
%hide Data.Linear.(.)
%hide Text.HTML.Node.a
%hide Types.SelectionMode.Select

||| Sets the `disabled` attribute of the given element
||| if the given values is not a `Valid`.
|||
||| This is useful for disabling components such as buttons
||| in the UI in case of invalid user input.
export %inline
disabledEdit : {0 a : _} -> Has JSErr es => Ref t -> EditRes a -> JS es ()
disabledEdit r = disabled r . not . isValid

endStream : DOMLocal => JSStream () -> JSStream t -> JSStream t
endStream end es =
  finally logEnded $ haltOn (P.observe' logAbort end) es

||| Adjusts a widget in such a way that its input streams ends
||| as soon as one of its nodes are removed from the DOM.
|||
||| This is used in utilities such as `bindEd` or `Web.Async.List`, where
||| external events decide when a node is removed from the UI.
export
endOnRemove : DOMLocal => Widget t -> JS es (Widget t)
endOnRemove (W ns es) = Prelude.do
  E end <- event ()
  pure $ W (map adj ns) (endStream end es)
  where
    adj : Sink () => HTMLNode -> HTMLNode
    adj (El  t as ns) = El  t (onRemove () :: as) ns
    adj (EEl t as)    = EEl t (onRemove () :: as)
    adj n             = n

--------------------------------------------------------------------------------
-- Text Widgets
--------------------------------------------------------------------------------

export
voidRef : Ref t -> Ref Void
voidRef (Id id)  = Elem id
voidRef (Elem s) = Elem s
voidRef Body     = Body
voidRef Document = Document
voidRef Window   = Window

||| Adds a unique ID to the given list of attributes if it does not yet
||| already have an ID attribute, and returns the updated list plus the ID.
export
attributesWithID :
     {auto lio : LIO io}
  -> {s   : _}
  -> (0 t : HTMLTag s)
  -> List (Attribute t)
  -> io (Ref t, List (Attribute t))
attributesWithID t attrs =
  case attrID attrs of
    Nothing => map (\i => (tagRef t i, ref i :: attrs)) uniqueID
    Just x  => pure (x, attrs)

||| Adds a unique ID to the given HTML node if it does not yet
||| already have an ID and returns the updated node plus its ID.
|||
||| Returns `Nothing` in case the node in question is a `Raw` node
||| or a `Text` node.
export
nodeWithID : LIO io => HTMLNode -> io (Maybe (Ref Void, HTMLNode))
nodeWithID (El t x y) =
  (\(i,a) => Just (voidRef i,El t a y)) <$> attributesWithID t x
nodeWithID (EEl t x)  =
  (\(i,a) => Just (voidRef i,EEl t a)) <$> attributesWithID t x
nodeWithID (Raw _)    = pure Nothing
nodeWithID (Text _)   = pure Nothing
nodeWithID Empty      = pure Nothing

||| Sets or unsets a custom validity message at the given node plus
||| sets a custom attribute (`data-validity`).
export
validateRes : DOMLocal => Ref t -> (0 p : ValidityTag t) => EditRes s -> Act ()
validateRes r v = validityMessage r (editRes v) >> attr r (validity v)

parameters {auto loc : DOMLocal}
           (tpe      : InputType)
           (attrs    : List (Attribute Tag.Input))

  textInP : String -> JS es (Ref Tag.Input, Widget String)
  textInP v = do
    E es   <- eventFrom v
    (i,as) <- attributesWithID Tag.Input attrs
    let es' := observe logInput es
    pure (i, W [input $ [value v, type tpe, onInput Prelude.id] ++ as] es')

  ||| An input element that emits `String` events.
  export
  textIn : String -> JS es (Widget String)
  textIn v = snd <$> textInP v

  ||| A validated input element that emits events of type
  ||| `EditRes e`.
  |||
  ||| A custom validity message is set in case of invalid input.
  export
  valIn : String -> (String -> EditRes e) -> JS es (Widget $ EditRes e)
  valIn v f = do
    (r, W n evs) <- textInP v
    pure $ W n (observe (validateRes r) (mapOutput f evs))

fakeBody : String -> String
fakeBody s =
  case [<] <>< forget (String.split ('\\' ==) s) of
    _ :< p => p
    _      => ""

toFile : InputInfo -> Maybe FileEv
toFile (MkInputInfo p [f] _) = Just (FE f (fakeBody p))
toFile _                     = Nothing

export
onFileIn : Sink e => (f : FileEv -> e) -> Attribute Tag.Input
onFileIn f = Event (Input $ map f . toFile)

Interpolation FileEv where interpolate = name

export
fileIn : DOMLocal => Attributes Tag.Input -> JS es (Widget $ EditRes FileEv)
fileIn as = do
  E es   <- eventFrom Missing
  pure $
    W [input $ [type File, onFileIn Valid] ++ as]
      (observe (logRes fileStr) es)

--------------------------------------------------------------------------------
-- Select Widgets
--------------------------------------------------------------------------------

listInit : Maybe t -> (v -> t) -> List v -> Maybe t
listInit (Just x) f xs     = Just x
listInit Nothing  f (x::_) = Just (f x)
listInit _        _ _      = Nothing

entriesInit : List (SelectEntry t) -> Maybe t
entriesInit []                = Nothing
entriesInit (Title _   :: xs) = entriesInit xs
entriesInit (Entry v _ :: xs) = Just v

selToRes : Maybe (SelectEv t) -> EditRes t
selToRes = maybe Missing (Valid . value)

initEv : Nat -> (t -> Bool) -> List (SelectEntry t) -> Maybe (SelectEv t)
initEv n f []                = Nothing
initEv n f (Title _   :: xs) = initEv n f xs
initEv n f (Entry v s :: xs) =
  case f v of
    False => initEv (S n) f xs
    True  => Just (SE n s v)

parameters {auto lc : DOMLocal}
           {auto eq : Eq t}

  ||| A select element displaying the values of type `v`
  ||| shown in the given list.
  |||
  ||| It fires events of type `t`, and uses two functions, one for
  ||| converting elements to events and one for displaying elements.
  export
  selEntries :
       List (SelectEntry t)
    -> List (Attribute Select)
    -> Maybe t
    -> JS es (Widget (EditRes t))
  selEntries vs as m = do
    let ini := m <|> entriesInit vs
    E ws @{s} <- eventFrom (initEv 0 ((ini ==) . Just) vs)
    let ms := cmap Just s
    let ws' := observe logSelect ws |> P.mapOutput selToRes
    pure $ W [selectEntries vs ((ini ==) . Just) id as] ws'

  ||| A select element displaying the values of type `v`
  ||| shown in the given list.
  |||
  ||| It fires events of type `t`, and uses two functions, one for
  ||| converting elements to events and one for displaying elements.
  export
  sel :
       (v -> t)
    -> (v -> String)
    -> List v
    -> List (Attribute Select)
    -> Maybe t
    -> JS es (Widget (EditRes t))
  sel f g = selEntries . map (\v => Entry (f v) (g v))

export %inline
txtEdit :
     {auto loc : DOMLocal}
  -> (parse : String -> EditRes t)
  -> (tpe   : InputType)
  -> (ini   : Maybe t -> String)
  -> (attrs : List (Attribute Tag.Input))
  -> Editor t
txtEdit parse tpe ini as = E $ \m => valIn tpe as (ini m) parse

export %inline
selEdit :
     {auto loc : DOMLocal}
  -> {auto ip : Interpolation t}
  -> {auto eq : Eq t}
  -> (values  : List t)
  -> (attrs   : List (Attribute Select))
  -> Editor t
selEdit vs = E . sel id interpolate vs

--------------------------------------------------------------------------------
-- Bound Editor
--------------------------------------------------------------------------------

0 StopRef : Type
StopRef = IORef (Maybe $ Event JS [JSErr] ())

%inline
stopref : Act StopRef
stopref = newref Nothing

%inline
stopStream : DOMLocal => StopRef -> Act (Event JS [JSErr] ())
stopStream ref = Prelude.do
  m <- readref ref
  for_ m $ \(E {}) => sink () >> logSwitchStopped
  ev  <- event ()
  writeref ref (Just ev)
  pure ev

hiddenDiv : DomID -> HTMLNode
hiddenDiv i = div [style [display None], ref i] []

export
bindEd : DOMLocal => (a -> Editor b) -> (Maybe b -> Maybe a) -> Editor a -> Editor b
bindEd f fromB (E w) =
  E $ \mb => Prelude.do
    i       <- uniqueID
    j       <- uniqueID
    ref     <- stopref
    W na as <- w (fromB mb)
    pure $ W (na ++ [hiddenDiv i, hiddenDiv j]) $
      switchMap id $
           P.mapMaybe toMaybe as
        |> P.zipWithIndex
        |> P.evalMap (adj i j ref mb)

  where
    adj :
         (i,j : DomID)
      -> StopRef
      -> Maybe b
      -> (a,Nat)
      -> Act (JSStream (EditRes b))
    adj i j ref mb (va,n)  = Prelude.do
      logSwitch
      E es <- stopStream ref
      W nb xs  <- widget (f va) (if n == 0 then mb else Nothing)
      replaceBetween (elemRef i) (elemRef j) nb
      logReplaced
      pure (endStream es xs)
