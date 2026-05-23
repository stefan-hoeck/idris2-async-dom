module Text.HTML.Select

import Data.List
import Data.Nat
import FS.Concurrent.Signal
import Text.HTML

%default total

public export
data SelectEntry : (t : Type) -> Type where
  Title : String    -> SelectEntry t
  Entry : (val : t) -> String -> SelectEntry t

public export
record SelectEv t where
  constructor SE
  index : Nat
  label : String
  value : t

export
Functor SelectEv where
  map f (SE i l v) = SE i l $ f v

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

selectIx : List (SelectEntry t) -> Nat -> Maybe (SelectEv t)
selectIx vs n = go vs n
  where
    go : List (SelectEntry t) -> Nat -> Maybe (SelectEv t)
    go []                _     = Nothing
    go (Entry v s :: xs) 0     = Just (SE n s v)
    go (Title _   :: xs) 0     = Nothing
    go (x :: xs)         (S k) = go xs k

app : {0 a : _} -> SnocList (a,List b) -> SnocList b -> a -> SnocList (a,List b)
app sx [<] x = sx
app sx sy  x = sx :< (x, sy <>> [])

groups :
     SnocList (String,List (Nat,t,String))
  -> SnocList (Nat,t,String)
  -> (lbl : String)
  -> Nat
  -> List (SelectEntry t)
  -> List (String,List (Nat,t,String))
groups sg sp l n []                = app sg sp l <>> []
groups sg sp l n (Title s   :: xs) = groups (app sg sp l) [<]  s (S n) xs
groups sg sp l n (Entry v s :: xs) = groups sg (sp :< (n,v,s)) l (S n) xs

export
selectEntries :
     {auto snk : Sink (SelectEv e)}
  -> (entries  : List (SelectEntry t))
  -> (sel      : t -> Bool)
  -> (toEvent  : t -> e)
  -> (attrs    : List (Attribute Select))
  -> HTMLNode
selectEntries es sel toEv attrs =
  select
    -- Note: The `change` event handler must be the last attribute
    --       otherwise it might already fire when the node is being
    --       set up.
    (attrs ++ [onChangeMaybe (map (map toEv) . selectIx es . cast)])
    (groups [<] [<] "" 0 es >>= grp)
  where
    opt : (Nat,t,String) -> HTMLNode
    opt (x,v,s) = option [value (show x), selected (sel v)] [Text s]

    grp : (String,List (Nat,t,String)) -> HTMLNodes
    grp ("",ps) = map opt ps
    grp (s,ps)  = [optgroup [label s] $ map opt ps]

||| Create a `<select>` element displaying the options in the given
||| list.
|||
||| @values  : the list of options
||| @sel     : true if the given item in the list should be selected
||| @display : how to display an option at the UI
||| @toEvent : how to convert an option to an event
||| @attrs   : additional attributes
export
selectFromListBy :
     {auto snk : Sink (SelectEv e)}
  -> (values   : List t)
  -> (sel      : t -> Bool)
  -> (display  : t -> String)
  -> (toEvent  : t -> e)
  -> (attrs    : List (Attribute Select))
  -> HTMLNode
selectFromListBy vs sel f = selectEntries ((\x => Entry x $ f x) <$> vs) sel

||| Like `selectFromListBy` but uses an optional initial value
||| to determine the initially selected value.
|||
||| @values  : the list of options
||| @init    : the initially selected option (if any)
||| @display : how to display an option at the UI
||| @toEvent : how to convert an option to an event
||| @attrs   : additional attributes
export
selectFromList :
     {auto snk : Sink (SelectEv e)}
  -> {auto eq : Eq t}
  -> (values  : List t)
  -> (init    : Maybe t)
  -> (display : t -> String)
  -> (toEvent : t -> e)
  -> (attrs   : List (Attribute Select))
  -> HTMLNode
selectFromList vs i = selectFromListBy vs ((i ==) . Just)

parameters {auto snk : Sink e}

  ||| Simplified version of `selectEntries` that does not take
  ||| a full `SelectEvent` sink.
  export %inline
  selectEntries' :
       (entries  : List (SelectEntry t))
    -> (sel      : t -> Bool)
    -> (toEvent  : t -> e)
    -> (attrs    : List (Attribute Select))
    -> HTMLNode
  selectEntries' = selectEntries @{cmap value snk}

  ||| Simplified version of `selectFromListBy` that does not take
  ||| a full `SelectEvent` sink.
  export %inline
  selectFromListBy' :
       (values   : List t)
    -> (sel      : t -> Bool)
    -> (display  : t -> String)
    -> (toEvent  : t -> e)
    -> (attrs    : List (Attribute Select))
    -> HTMLNode
  selectFromListBy' = selectFromListBy @{cmap value snk}

  ||| Simplified version of `selectFromList` that does not take
  ||| a full `SelectEvent` sink.
  export %inline
  selectFromList' :
       {auto eq : Eq t}
    -> (values  : List t)
    -> (init    : Maybe t)
    -> (display : t -> String)
    -> (toEvent : t -> e)
    -> (attrs   : List (Attribute Select))
    -> HTMLNode
  selectFromList' = selectFromList @{cmap value snk}
