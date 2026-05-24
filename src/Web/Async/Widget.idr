module Web.Async.Widget

import Data.Linear.Unique
import Data.List1
import Data.String
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
  finally logEnded $ haltOn (P.observe' logRemove end) es

||| Adjusts a widget in such a way that its input streams ends
||| as soon as its node is removed from the DOM.
|||
||| This is used in utilities such as `bindEd` or `Web.Async.List`, where
||| external events decide when a node is removed from the UI.
export
endOnRemove : DOMLocal => Widget t -> JS es (Widget t)
endOnRemove (W (El t as ns) es) = Prelude.do
  E end <- event ()
  pure $ W (El t (onRemove () :: as) ns) (endStream end es)
endOnRemove (W (EEl t as) es) = Prelude.do
  E end <- event ()
  pure $ W (EEl t $ onRemove () :: as) (endStream end es)
endOnRemove w = pure w

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

parameters {auto loc : DOMLocal}
           (tpe      : InputType)
           (attrs    : List (Attribute Tag.Input))

  textInP : String -> JS es (Ref Tag.Input, Widget String)
  textInP v = do
    E es   <- eventFrom v
    (i,as) <- attributesWithID Tag.Input attrs
    let es' := observe logInput es
    pure (i, W (input $ [value v, type tpe, onInput Prelude.id] ++ as) es')

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
    pure $ W n (observe (validate r . toEither) (mapOutput f evs))

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
  E es <- eventFrom Missing
  pure $ W (input $ [type File, onFileIn Valid]++as) (observe (logRes fileStr) es)

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
    pure $ W (selectEntries vs ((ini ==) . Just) id as) ws'

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

export
bindEd :
     {auto loc : DOMLocal}
  -> (cls  : Class)
  -> (wrap : (fst,snd : HTMLNode) -> HTMLNode)
  -> (a -> Editor b)
  -> (Maybe b -> a)
  -> Editor a
  -> Editor b
bindEd cls wrap f fromB (E w) =
  E $ \mb => Prelude.do
    i       <- uniqueID
    W na as <- w (Just $ fromB mb)
    W nb bs <- widget (f $ fromB mb) mb >>= wdgt i
    pure $ W (wrap na nb) $
      switchMap id $ cons bs $
        P.tail as |> P.mapMaybe toMaybe |> P.evalMap (adj i)

  where
    wdgt : {0 t : _} -> DomID -> Widget t -> Act (Widget t)
    wdgt i = endOnRemove . adjNode (\n => div [ref i, class cls] [n])

    adj : DomID -> a -> Act (JSStream (EditRes b))
    adj i va  = Prelude.do
      W nb xs  <- widget (f va) Nothing >>= wdgt i
      replace (elemRef i) nb
      pure xs
