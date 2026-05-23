module Web.Async.Widget.Types

import Derive.Prelude
import Monocle
import Text.HTML
import Web.Async.Util
import Web.Internal.Types

%default total
%language ElabReflection
%hide Data.Linear.(.)
%hide Text.HTML.Node.a

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
toMaybe : EditRes t -> Maybe t
toMaybe (Valid v) = Just v
toMaybe _         = Nothing

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
  Valid f   <*> v  = map f v
  Invalid x <*> _  = Invalid x
  Missing   <*> _  = Missing

export
Monad EditRes where
  Missing   >>= _ = Missing
  Invalid x >>= _ = Invalid x
  Valid x   >>= f = f x

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

export
adjNode : (HTMLNode -> HTMLNode) -> Widget e -> Widget e
adjNode f = {node $= f}

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
-- File Input
--------------------------------------------------------------------------------

public export
record FileEv where
  constructor FE
  file : File
  name : String

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
|||
||| A couple of notes about how an editor is supposed to behave:
|||   * If the initial value used for creating the widget is a `Nothing`,
|||     the stream of values produced by the widget *may* already hold
|||     a valid default value. In case no sensible default is available,
|||     the stream's initial value *should* be `Missing`.
|||   * If the initial value used for creating the widget is a `Just`,
|||     the stream of values produced by the widget *must* emit a `Valid`
|||     wrapping the provided initial value as its first output.
|||
||| The above two rules make sure an editor behaves as expected, especially
||| when combining several editors in a form, or using the experimental
||| `bindEd` combinator.
public export
record Editor (t : Type) where
  constructor E
  ||| Create a node and stream of values from an optional initial value.
  widget : Maybe t -> Act (Widget $ EditRes t)

export
adjEditor :
     (Maybe a -> Maybe b)
  -> (EditRes b -> EditRes a)
  -> Editor b
  -> Editor a
adjEditor adjm adjres ed =
  E $ \mb => Prelude.do
    W n bs <- ed.widget (adjm mb)
    pure $ W n $ P.mapOutput adjres bs

||| Views an editor through an isomorphism.
export
editI : Iso' t1 t2 -> Editor t1 -> Editor t2
editI i (E f) = E $ map3 i.get_ . f . map i.reverseGet

||| Views the `new` values of an editor through a prism.
export
editP : Prism' t2 t1 -> Editor t1 -> Editor t2
editP p (E f) = E $ map3 p.reverseGet . f . (>>= first p)

||| Refines an editor to produce values of a more restricted type.
export
refineEdit :
     (t2 -> Maybe t1)
  -> (refine : t1 -> EditRes t2)
  -> Editor t1
  -> Editor t2
refineEdit ini f (E w) = E $ \m => map (>>= f) <$> w (m >>= ini)

export %inline
mapEvents : (JSStream (EditRes t) -> JSStream (EditRes t)) -> Editor t -> Editor t
mapEvents f (E w) = E $ map {events $= f} . w

||| A dummy `Editor` for uneditable values.
|||
||| The given value is fired exactly once.
public export
dummy : t -> Editor t
dummy v = E $ \_ => pure (once (Valid v))
