module Web.Async.List

import Data.Linear.Traverse1
import Data.Linear.Unique
import Data.List1
import Derive.Prelude
import Text.HTML.DomID
import Web.Async.Form

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- List Event
--------------------------------------------------------------------------------

public export
data ListEv = Prepend | Append

%runElab derive "ListEv" [Show,Eq,Ord]

--------------------------------------------------------------------------------
-- List Updates
--------------------------------------------------------------------------------

0 ST : Type -> Type
ST t = List (Token World, EditRes t)

missing : ST t -> EditRes (List t)
missing []        = Missing
missing (x :: xs) =
  case snd x of
    Invalid err => Invalid err
    _           => missing xs

travres : SnocList t -> ST t -> EditRes (List t)
travres sx []        = Valid $ sx <>> []
travres sx (x :: xs) =
  case snd x of
    Missing     => missing xs
    Invalid err => Invalid err
    Valid val   => travres (sx :< val) xs

del : Token World -> ST t -> ST t
del n = filter ((n /=) . fst)

upd : Token World -> EditRes t -> ST t -> ST t
upd tok res = map (\p => if fst p == tok then (tok,res) else p)

new : Token World -> ListEv -> ST t -> ST t
new tok Prepend xs = (tok,Missing)::xs
new tok Append  xs = xs ++ [(tok,Missing)]

--------------------------------------------------------------------------------
-- List Editor
--------------------------------------------------------------------------------

delete : Token World -> JSStream () -> Ref Void -> JSStream (ST t -> ST t)
delete t us r = P.take 1 us |> P.evalMap (\_ => remove r $> del t)

initial : Maybe (List t) -> IO1 (List (Token World, t))
initial Nothing   = ([] #)
initial (Just ts) =
  traverse1 (\v,t => let tok # t := token1 t in (tok,v) # t) ts

parameters {default 0xffff_fffe limit : Nat}
           (parent : Act (Ref Void, HTMLNodes -> Widget ListEv))
           (row    : HTMLNode -> Act (Ref Void, Widget ()))
           (ed     : Editor t)

  rw : Token World -> Maybe t -> Act (Widget $ ST t -> ST t)
  rw tok ini = do
    W n s           <- ed.widget ini
    (rid, W n2 del) <- row n
    pure $ W n2 $ mergeHaltL (delete tok del rid) (P.mapOutput (upd tok) s)

  add : Ref Void -> ListEv -> Act (JSStream (ST t -> ST t))
  add rpar le = do
    tok   <- token
    W n s <- rw tok Nothing
    if le == Append then append rpar n else prepend rpar n
    pure $ emit (new tok le) <+> s

  ||| An editor for lists of values.
  |||
  ||| This uses `row` to wrap a node for editing a single value
  ||| in a node with some kind of *delete* or *remove* function.
  |||
  ||| All rows are displayed in the `parent` node.
  export
  editList : Editor (List t)
  editList =
    E $ \m => do
      (ref, makeW) <- parent
      ini          <- lift1 $ initial m
      rows         <- traverseList (\(t,v) => rw t (Just v)) ini
      let W n listEvs := makeW (map node rows)
      pure $ W n $
        (emits (map events rows) <+> evalMap (add ref) listEvs)
        |> parJoin (S limit)
        |> scanFrom1 (map2 Valid ini)
        |> P.mapOutput (travres [<])

||| Like `editList` but for non-empty lists.
export
editList1 : Editor (List t) -> Editor (List1 t)
editList1 =
  refineEdit (Just . forget) $ \case
    x::xs => Valid $ x:::xs
    []    => Missing
