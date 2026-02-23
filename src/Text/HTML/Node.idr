module Text.HTML.Node

import JS
import Data.String
import FS.Concurrent.Signal
import Text.HTML.Attribute
import Text.HTML.Event
import Text.HTML.Ref
import Text.HTML.Tag

%default total

public export
data HTMLNode : Type where
  El    :  {tag   : String}
        -> (0 tpe : HTMLTag tag)
        -> List (Attribute tpe)
        -> List HTMLNode
        -> HTMLNode

  ||| Data constructor for empty elements
  EEl   :  {tag   : String}
        -> (0 tpe : HTMLTag tag)
        -> List (Attribute tpe)
        -> HTMLNode

  Raw   : String -> HTMLNode

  Text  : String -> HTMLNode

  Empty : HTMLNode

public export
0 HTMLNodes : Type
HTMLNodes = List HTMLNode

export %inline
FromString HTMLNode where
  fromString = Text

||| True if the given node is the empty node
export %inline
isEmpty : HTMLNode -> Bool
isEmpty Empty = True
isEmpty _     = False

||| True if the given node is not the empty node
export %inline
nonEmpty : HTMLNode -> Bool
nonEmpty = not . isEmpty

||| An optional node that is set to `Empty` if the given `Bool` is `False`.
export
nodeIf : Bool -> Lazy HTMLNode -> HTMLNode
nodeIf True n  = n
nodeIf False _ = Empty

||| Converts an optional value to a `HTMLNode`
export %inline
nodeMaybe : (a -> HTMLNode) -> Maybe a -> HTMLNode
nodeMaybe f = maybe Empty f

||| Prepend a non-event attribute to a node's list of attributes.
export
withAttribute : ({0 s : _} -> {0 t : HTMLTag s} -> Attribute t) -> HTMLNode -> HTMLNode
withAttribute a (El tp as ns) = El tp (a ::as) ns
withAttribute a (EEl tp as)   = EEl tp (a ::as)
withAttribute a n             = n

||| Prepend the given ID to a node's list of attributes.
export
withId : String -> HTMLNode -> HTMLNode
withId s (El tp as ns) = El tp (Id (Id s) :: as) ns
withId s (EEl tp as)   = EEl tp (Id (Id s) :: as)
withId s n             = n

||| Prepend the given event to a node's list of attributes.
export
withEv : Sink e => DOMEvent e -> HTMLNode -> HTMLNode
withEv ev (El tp as ns) = El tp (Event ev :: as) ns
withEv ev (EEl tp as)   = EEl tp (Event ev :: as)
withEv ev n             = n

export %inline
a : List (Attribute A) -> HTMLNodes -> HTMLNode
a = El _

export %inline
address : List (Attribute Address) -> HTMLNodes -> HTMLNode
address = El _

export %inline
area : List (Attribute Area) -> HTMLNode
area = EEl _

export %inline
article : List (Attribute Article) -> HTMLNodes -> HTMLNode
article = El _

export %inline
aside : List (Attribute Aside) -> HTMLNodes -> HTMLNode
aside = El _

export %inline
audio : List (Attribute Audio) -> HTMLNodes -> HTMLNode
audio = El _

export %inline
base : List (Attribute Base) -> HTMLNode
base = EEl _

export %inline
blockquote : List (Attribute Blockquote) -> HTMLNodes -> HTMLNode
blockquote = El _

export %inline
body : List (Attribute Tag.Body) -> HTMLNodes -> HTMLNode
body = El _

export %inline
br : List (Attribute Br) -> HTMLNode
br = EEl _

export %inline
button : List (Attribute Tag.Button) -> HTMLNodes -> HTMLNode
button = El _

export %inline
canvas : List (Attribute Canvas) -> HTMLNodes -> HTMLNode
canvas = El _

export %inline
caption : List (Attribute Caption) -> HTMLNodes -> HTMLNode
caption = El _

export %inline
col : List (Attribute Col) -> HTMLNode
col = EEl _

export %inline
colgroup : List (Attribute Colgroup) -> HTMLNodes -> HTMLNode
colgroup = El _

export %inline
data_ : List (Attribute Data) -> HTMLNodes -> HTMLNode
data_ = El _

export %inline
datalist : List (Attribute Datalist) -> HTMLNodes -> HTMLNode
datalist = El _

export %inline
del : List (Attribute Del) -> HTMLNodes -> HTMLNode
del = El _

export %inline
details : List (Attribute Details) -> HTMLNodes -> HTMLNode
details = El _

export %inline
dialog : List (Attribute Dialog) -> HTMLNodes -> HTMLNode
dialog = El _

export %inline
div : List (Attribute Div) -> HTMLNodes -> HTMLNode
div = El _

export %inline
dl : List (Attribute Dl) -> HTMLNodes -> HTMLNode
dl = El _

export %inline
embed : List (Attribute Embed) -> HTMLNode
embed as = El _ as []

export %inline
fieldset : List (Attribute FieldSet) -> HTMLNodes -> HTMLNode
fieldset = El _

export %inline
footer : List (Attribute Footer) -> HTMLNodes -> HTMLNode
footer = El _

export %inline
form : List (Attribute Form) -> HTMLNodes -> HTMLNode
form = El _

export %inline
h1 : List (Attribute H1) -> HTMLNodes -> HTMLNode
h1 = El _

export %inline
h2 : List (Attribute H2) -> HTMLNodes -> HTMLNode
h2 = El _

export %inline
h3 : List (Attribute H3) -> HTMLNodes -> HTMLNode
h3 = El _

export %inline
h4 : List (Attribute H4) -> HTMLNodes -> HTMLNode
h4 = El _

export %inline
h5 : List (Attribute H5) -> HTMLNodes -> HTMLNode
h5 = El _

export %inline
h6 : List (Attribute H6) -> HTMLNodes -> HTMLNode
h6 = El _

export %inline
header : List (Attribute Header) -> HTMLNodes -> HTMLNode
header = El _

export %inline
hr : List (Attribute HR) -> HTMLNode
hr = EEl _

export %inline
html : List (Attribute Html) -> HTMLNodes -> HTMLNode
html = El _

export %inline
iframe : List (Attribute IFrame) -> HTMLNodes -> HTMLNode
iframe = El _

export %inline
img : List (Attribute Img) -> HTMLNode
img = EEl _

export %inline
input : List (Attribute Tag.Input) -> HTMLNode
input = EEl _

export %inline
ins : List (Attribute Ins) -> HTMLNodes -> HTMLNode
ins = El _

export %inline
label : List (Attribute Label) -> HTMLNodes -> HTMLNode
label = El _

export %inline
legend : List (Attribute Legend) -> HTMLNodes -> HTMLNode
legend = El _

export %inline
li : List (Attribute Li) -> HTMLNodes -> HTMLNode
li = El _

export %inline
link : List (Attribute Link) -> HTMLNode
link = EEl _

export %inline
map : List (Attribute Tag.Map) -> HTMLNodes -> HTMLNode
map = El _

export %inline
menu : List (Attribute Menu) -> HTMLNodes -> HTMLNode
menu = El _

export %inline
meta : List (Attribute Meta) -> HTMLNode
meta = EEl _

export %inline
meter : List (Attribute Meter) -> HTMLNodes -> HTMLNode
meter = El _

export %inline
object : List (Attribute Tag.Object) -> HTMLNodes -> HTMLNode
object = El _

export %inline
ol : List (Attribute Ol) -> HTMLNodes -> HTMLNode
ol = El _

export %inline
optgroup : List (Attribute OptGroup) -> HTMLNodes -> HTMLNode
optgroup = El _

export %inline
option : List (Attribute Option) -> HTMLNodes -> HTMLNode
option = El _

export %inline
output : List (Attribute Output) -> HTMLNodes -> HTMLNode
output = El _

export %inline
p : List (Attribute P) -> HTMLNodes -> HTMLNode
p = El _

export %inline
param : List (Attribute Param) -> HTMLNode
param = EEl _

export %inline
picture : List (Attribute Picture) -> HTMLNodes -> HTMLNode
picture = El _

export %inline
pre : List (Attribute Pre) -> HTMLNodes -> HTMLNode
pre = El _

export %inline
progress : List (Attribute Progress) -> HTMLNodes -> HTMLNode
progress = El _

export %inline
q : List (Attribute Q) -> HTMLNodes -> HTMLNode
q = El _

export %inline
script : List (Attribute Script) -> HTMLNodes -> HTMLNode
script = El _

export %inline
section : List (Attribute Section) -> HTMLNodes -> HTMLNode
section = El _

export %inline
select : List (Attribute Select) -> HTMLNodes -> HTMLNode
select = El _

export %inline
slot : List (Attribute Slot) -> HTMLNodes -> HTMLNode
slot = El _

export %inline
source : List (Attribute Source) -> HTMLNode
source as = El _ as []

export %inline
span : List (Attribute Span) -> HTMLNodes -> HTMLNode
span = El _

export %inline
style : List (Attribute Style) -> HTMLNodes -> HTMLNode
style = El _

export %inline
table : List (Attribute Table) -> HTMLNodes -> HTMLNode
table = El _

export %inline
tbody : List (Attribute Tbody) -> HTMLNodes -> HTMLNode
tbody = El _

export %inline
td : List (Attribute Td) -> HTMLNodes -> HTMLNode
td = El _

export %inline
template : List (Attribute Template) -> HTMLNodes -> HTMLNode
template = El _

export %inline
textarea : List (Attribute TextArea) -> HTMLNodes -> HTMLNode
textarea = El _

export %inline
tfoot : List (Attribute Tfoot) -> HTMLNodes -> HTMLNode
tfoot = El _

export %inline
th : List (Attribute Th) -> HTMLNodes -> HTMLNode
th = El _

export %inline
thead : List (Attribute Thead) -> HTMLNodes -> HTMLNode
thead = El _

export %inline
time : List (Attribute Tag.Time) -> HTMLNodes -> HTMLNode
time = El _

export %inline
title : List (Attribute Title) -> HTMLNodes -> HTMLNode
title = El _

export %inline
tr : List (Attribute Tr) -> HTMLNodes -> HTMLNode
tr = El _

export %inline
track : List (Attribute Track) -> HTMLNode
track = EEl _

export %inline
ul : List (Attribute Ul) -> HTMLNodes -> HTMLNode
ul = El _

export %inline
video : List (Attribute Video) -> HTMLNodes -> HTMLNode
video = El _

--------------------------------------------------------------------------------
--          Rendering Html
--------------------------------------------------------------------------------

export
escape : String -> String
escape = fastConcat . map esc . unpack

  where
    esc : Char -> String
    esc '<'          = "&lt;"
    esc '>'          = "&gt;"
    esc '&'          = "&amp;"
    esc '"'          = "&quot;"
    esc '\''         = "&#x27"
    esc '\n'         = "\n"
    esc '\r'         = "\r"
    esc '\t'         = "\t"
    esc c            = if c < ' ' then "" else singleton c

attrs : List (Attribute t) -> String
attrs as = let s = displayAttributes as in if null s then "" else " " ++ s

export
render : HTMLNode -> String
render n = case n of
  Raw x             => x
  Text x            => escape x
  EEl {tag} _ as    => "<\{tag}\{attrs as}>"
  El  {tag} _ as ns => "<\{tag}\{attrs as}>\{go [<] ns}</\{tag}>"
  Empty             => ""

  where
    go : SnocList String -> HTMLNodes -> String
    go ss (n :: ns) = go (ss :< render n) ns
    go ss []        = concat $ ss <>> []

export
renderMany : HTMLNodes -> String
renderMany = fastConcat . map render
