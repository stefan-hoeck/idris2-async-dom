module Web.Async.Form

import public Control.Barbie
import public Data.Singleton
import public Monocle
import public Web.Async
import public Web.Async.Widget
import Text.HTML.DomID

%default total

||| An editable field in a web form consisting of the field's
||| name, the node for data input, and an ID that will be used
||| to send validation messages to a label.
public export
record FormField where
  constructor FF
  name : String
  node : HTMLNode
  err  : Ref Label

toField : (String,Widget t, Ref Label) -> Maybe FormField
toField (nm, W Empty _, _) = Nothing
toField (nm, W n _, r)     = Just (FF nm n r)

formStream : List (String, Widget (t -> t),Ref Label) -> t -> JSStream t
formStream ps ini =
  merge (map (events . fst . snd) ps) |> scans1 ini (\x,f => f x)

parameters {0 f        : Type}
           {auto ipf   : Interpolation f}
           (0 rec      : (f -> Type) -> Type)
           (formNode   : List FormField -> Act HTMLNode)
           (valid      : {0 t : _} -> Ref Label -> EditRes t -> Act ())
           {0 g        : f -> Type}
           {auto sings : rec Singleton}
           {auto fuc   : FunctorB f rec}
           {auto app   : ApplicativeB f rec}
           {auto trv   : TraversableB f rec}
           {auto rrd   : RecordB f rec}

  -- type of a single form widget: the field's name, the widget itself,
  -- which emits functions used for updating the barbie record,
  -- and a label ID used for writing error messages.
  0 WForm : Type
  WForm = (String,Widget (rec (EditRes . g) -> rec (EditRes . g)),Ref Label)

  -- Creates the information for editing a single
  -- field of the record.
  --
  -- The properly typed `Singleton` is required to create the
  -- lens for reading and updating the record field (see interface `RecordB`)
  editField :
       {0 v : f}
    -> Singleton v
    -> Editor (g v)
    -> Maybe (rec g)
    -> Act WForm
  editField (Val v) (E fun) mrec = do
    -- create a unique ID used for error messages
    ref   <- uniqueRef Label

    -- create the HTML node and input stream
    W n s <- fun $ map (field g v).get_ mrec

    -- adjust the input stream so that validation messages
    -- are sent to `ref` and all input is used to update
    -- the corresponding field of the barbie
    let s2 := s |> observe (valid ref) |> mapOutput (set (field' v))

    pure $ (interpolate v, W n s2, ref)

  ||| An editable web form where the different fields of a
  ||| record can be edited and validated.
  |||
  ||| The record in question must be a *barbie*
  ||| (see the corresponding library), and this function takes as
  ||| input such a record of editors and returns an editor of
  ||| the record.
  |||
  ||| @ `rec`      : the barbie record type used to group the
  |||                field values.
  ||| @ `formNode` : used to group and display the different
  |||                form fields in a single HTML node.
  |||
  ||| @ `validate` : used send validation messages to a label or similar
  |||
  ||| Note: For uneditable record fields that should not appear in the
  |||       user interface, use a `dummy` editor.
  export
  form : rec (Editor . g) -> Editor (rec g)
  form edits =
       -- convert all value editors to record updaters
   let eflds   := bzipWith editField sings edits

       -- initial value of the stream: all fields are missing
       -- (this will automatically be updated with the initial event of every
       -- input stream when `merge`ing them, so that non-mandatory field values
       -- will no longer be missing)
       missAll := the (rec (EditRes . g)) (bpure @{app} Missing)
    in E $ \recm => do
         -- sets up (in `io`) all editor widgets by applying
         -- them to the optional initial record `recm`
         recw   <- btraverse (flip apply recm) eflds

         -- extract the editor triples as a list for
         let ws := bfoldMap pure recw

         -- group the editing fields in a single HTML node
         node   <- formNode (mapMaybe toField ws)

         pure $ W node (formStream ws missAll |> mapOutput bsequence)
