module Web.Async.Widget

import Data.Linear.Unique
import Derive.Prelude
import Monocle
import Text.HTML
import Text.HTML.DomID
import Text.HTML.Select
import Web.Async.Util
import Web.Async.View

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- EditRes
--------------------------------------------------------------------------------

||| Result of editing some kind of input element.
|||
||| Input is either missing, invalid, or valid.
public export
data EditRes : Type -> Type where
  Missing : EditRes t
  Invalid : (err : String) -> EditRes t
  Valid   : (val : t) -> EditRes t

%runElab derive "EditRes" [Show,Eq]

export
toEither : EditRes t -> Either String ()
toEither (Invalid s) = Left s
toEither _           = Right ()

export
isValid : EditRes t -> Bool
isValid (Valid _) = True
isValid _         = False

export
Functor EditRes where
  map f Missing     = Missing
  map f (Invalid v) = Invalid v
  map f (Valid v)   = Valid $ f v

export
Applicative EditRes where
  pure = Valid
  Valid f   <*> Valid v   = Valid (f v)
  Invalid x <*> _         = Invalid x
  _         <*> Invalid x = Invalid x
  _         <*> _         = Missing

--------------------------------------------------------------------------------
-- Widgets
--------------------------------------------------------------------------------

||| A `Widget e` is an interactive UI element that emits
||| events of type `e`.
public export
record Widget e where
  constructor W
  node   : HTMLNode
  events : JSStream e

||| A dummy widget without a node representation that keeps
||| producing the given value.
export
constant : t -> Widget t
constant = W Empty . fill

||| A dummy widget without a node representation that
||| never fires an event.
export
empty : Widget t
empty = W Empty (pure ())

||| A dummy widget without a node representation that
||| fires the given event exactly once.
export
once : t -> Widget t
once = W Empty . emit

export
Functor Widget where
  map f (W n p) = W n $ mapOutput f p

||| Sets the `disabled` attribute of the given element
||| if the given values is not a `Valid`.
|||
||| This is useful for disabling components such as buttons
||| in the UI in case of invalid user input.
export %inline
disabledEdit : {0 a : _} -> Has JSErr es => Ref t -> EditRes a -> JS es ()
disabledEdit r = disabled r . not . isValid

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
||| already have an ID attribute, andreturns the updated list plus the ID.
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

parameters {auto lio : LIO io}
           (tpe      : InputType)
           (attrs    : List (Attribute Tag.Input))

  textInP : String -> io (Ref Tag.Input, Widget String)
  textInP v = do
    s      <- signal v
    (i,as) <- attributesWithID Tag.Input attrs
    pure
      ( i
      , W (input $ [value v, type tpe, onInput Prelude.id] ++ as) (discrete s)
      )

  ||| An input element that emits `String` events.
  export
  textIn : String -> io (Widget String)
  textIn v = snd <$> textInP v

  ||| A validated input element that emits events of type
  ||| `EditRes e`.
  |||
  ||| A custom validity message is set in case of invalid input.
  export
  valIn : String -> (String -> EditRes e) -> io (Widget $ EditRes e)
  valIn v f = do
    (r, W n evs) <- textInP v
    pure $ W n (observe (validate r . toEither) (mapOutput f evs))

--------------------------------------------------------------------------------
-- Select Widgets
--------------------------------------------------------------------------------

listInit : Maybe t -> (v -> t) -> List v -> Maybe t
listInit (Just x) f xs     = Just x
listInit Nothing  f (x::_) = Just (f x)
listInit _        _ _      = Nothing

parameters {auto lio : LIO io}
           {auto eq  : Eq t}

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
    -> io (Widget (EditRes t))
  sel f g vs as m = do
    let ini := listInit m f vs
    s <- signal (maybe Missing Valid ini)
    pure $ W
      (selectFromListBy vs ((ini ==) . Just . f) g (Valid . f) as)
      (discrete s)

--------------------------------------------------------------------------------
-- Editor
--------------------------------------------------------------------------------

parameters {auto ff : Functor f}
           {auto fg : Functor g}

  export %inline
  map2 : (x -> y) -> f (g x) -> f (g y)
  map2 = map . map

  export %inline
  map3 : Functor h => (x -> y) -> f (g (h x)) -> f (g (h y))
  map3 = map . map . map

||| An `Editor` describes how to create new interactive DOM elements that
||| typically serve as a form of (validated) user input. It consists of an
||| `HTMLNode` for displaying the editing form plus a stream of validated
||| input data.
|||
||| An editor and its corresponding widgets can be something simple like a
||| text input field or a `<select>` element, or it can be highly complex
||| like a canvas and a group of DOM elements for editing molecules.
public export
record Editor (t : Type) where
  constructor E
  ||| Create a node and stream of values from an optional initial value.
  widget : Maybe t -> Act (Widget $ EditRes t)

||| Views an editor through an isomorphism.
export
editI : Iso' t1 t2 -> Editor t1 -> Editor t2
editI i (E f) = E $ map3 i.get_ . f . map i.reverseGet

||| Views the `new` values of an editor through a prism.
export
editP : Prism' t2 t1 -> Editor t1 -> Editor t2
editP p (E f) = E $ map3 p.reverseGet . f . (>>= first p)

export %inline
mapEvents : (JSStream (EditRes t) -> JSStream (EditRes t)) -> Editor t -> Editor t
mapEvents f (E w) = E $ map {events $= f} . w

||| A dummy `Editor` for uneditable values.
|||
||| The given value is fired exactly once.
public export
dummy : t -> Editor t
dummy v = E $ \_ => pure (once (Valid v))

export
txtEdit :
     {auto ip : Interpolation t}
  -> (parse   : String -> EditRes t)
  -> (attrs   : List (Attribute Tag.Input))
  -> Editor t
txtEdit parse as =
  E $ \m => valIn Text as (maybe "" interpolate m) parse

export %inline
selEdit :
     {auto ip : Interpolation t}
  -> {auto eq : Eq t}
  -> (values  : List t)
  -> (attrs   : List (Attribute Select))
  -> Editor t
selEdit vs = E . sel id interpolate vs
