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

attrID : List (Attribute t) -> Maybe (Ref t)
attrID []           = Nothing
attrID (Id r :: _)  = Just r
attrID (_    :: xs) = attrID xs

||| Generates a unique ID to be used to identify a DOM element.
export
uniqueID : LIO io => io DomID
uniqueID = map (D . ("uid" ++) . show) token

||| Uses `uniqueID` to generate a reference for a DOM element.
export
uniqueRef : {s : _} -> LIO io => (0 t : HTMLTag s) -> io (Ref t)
uniqueRef tag = tagRef tag <$> uniqueID

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

--------------------------------------------------------------------------------
-- Text Widgets
--------------------------------------------------------------------------------

parameters {auto lio : LIO io}
           (tpe      : InputType)
           (attrs    : List (Attribute Tag.Input))

  withID : io (Ref Tag.Input, List (Attribute Tag.Input))
  withID =
    case attrID attrs of
      Nothing => map (\i => (tagRef _ i, ref i :: attrs)) uniqueID
      Just x  => pure (x, attrs)

  textInP : String -> io (Ref Tag.Input, Widget String)
  textInP v = do
    s      <- signal v
    (i,as) <- withID
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
