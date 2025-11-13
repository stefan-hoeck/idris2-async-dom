module Text.HTML.Node

import JS
import Data.String
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

  Raw   : String -> HTMLNode

  Text  : String -> HTMLNode

  Empty : HTMLNode

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

||| Prepend a non-event attribute to a node's list of attributes.
export
withAttribute : ({0 s : _} -> {0 t : HTMLTag s} -> Attribute t) -> HTMLNode -> HTMLNode
withAttribute a (El tp as ns) = El tp (a ::as) ns
withAttribute a n             = n

||| Prepend the given ID to a node's list of attributes.
export
withId : String -> HTMLNode -> HTMLNode
withId s (El tp as ns) = El tp (Id (Id s) :: as) ns
withId s n             = n

||| Prepend the given event to a node's list of attributes.
export
withEv : Sink e => DOMEvent e -> HTMLNode -> HTMLNode
withEv ev (El tp as ns) = El tp (Event ev :: as) ns
withEv ev n             = n

export %inline
a : List (Attribute A) -> List HTMLNode -> HTMLNode
a = El _

export %inline
address : List (Attribute Address) -> List HTMLNode -> HTMLNode
address = El _

export %inline
area : List (Attribute Area) -> List HTMLNode -> HTMLNode
area = El _

export %inline
article : List (Attribute Article) -> List HTMLNode -> HTMLNode
article = El _

export %inline
aside : List (Attribute Aside) -> List HTMLNode -> HTMLNode
aside = El _

export %inline
audio : List (Attribute Audio) -> List HTMLNode -> HTMLNode
audio = El _

export %inline
base : List (Attribute Base) -> List HTMLNode -> HTMLNode
base = El _

export %inline
blockquote : List (Attribute Blockquote) -> List HTMLNode -> HTMLNode
blockquote = El _

export %inline
body : List (Attribute Tag.Body) -> List HTMLNode -> HTMLNode
body = El _

export %inline
br : List (Attribute Br) -> List HTMLNode -> HTMLNode
br = El _

export %inline
button : List (Attribute Tag.Button) -> List HTMLNode -> HTMLNode
button = El _

export %inline
canvas : List (Attribute Canvas) -> List HTMLNode -> HTMLNode
canvas = El _

export %inline
caption : List (Attribute Caption) -> List HTMLNode -> HTMLNode
caption = El _

export %inline
col : List (Attribute Col) -> List HTMLNode -> HTMLNode
col = El _

export %inline
colgroup : List (Attribute Colgroup) -> List HTMLNode -> HTMLNode
colgroup = El _

export %inline
data_ : List (Attribute Data) -> List HTMLNode -> HTMLNode
data_ = El _

export %inline
datalist : List (Attribute Datalist) -> List HTMLNode -> HTMLNode
datalist = El _

export %inline
del : List (Attribute Del) -> List HTMLNode -> HTMLNode
del = El _

export %inline
details : List (Attribute Details) -> List HTMLNode -> HTMLNode
details = El _

export %inline
dialog : List (Attribute Dialog) -> List HTMLNode -> HTMLNode
dialog = El _

export %inline
div : List (Attribute Div) -> List HTMLNode -> HTMLNode
div = El _

export %inline
dl : List (Attribute Dl) -> List HTMLNode -> HTMLNode
dl = El _

export %inline
embed : List (Attribute Embed) -> List HTMLNode -> HTMLNode
embed = El _

export %inline
fieldset : List (Attribute FieldSet) -> List HTMLNode -> HTMLNode
fieldset = El _

export %inline
footer : List (Attribute Footer) -> List HTMLNode -> HTMLNode
footer = El _

export %inline
form : List (Attribute Form) -> List HTMLNode -> HTMLNode
form = El _

export %inline
h1 : List (Attribute H1) -> List HTMLNode -> HTMLNode
h1 = El _

export %inline
h2 : List (Attribute H2) -> List HTMLNode -> HTMLNode
h2 = El _

export %inline
h3 : List (Attribute H3) -> List HTMLNode -> HTMLNode
h3 = El _

export %inline
h4 : List (Attribute H4) -> List HTMLNode -> HTMLNode
h4 = El _

export %inline
h5 : List (Attribute H5) -> List HTMLNode -> HTMLNode
h5 = El _

export %inline
h6 : List (Attribute H6) -> List HTMLNode -> HTMLNode
h6 = El _

export %inline
header : List (Attribute Header) -> List HTMLNode -> HTMLNode
header = El _

export %inline
hr : List (Attribute HR) -> List HTMLNode -> HTMLNode
hr = El _

export %inline
html : List (Attribute Html) -> List HTMLNode -> HTMLNode
html = El _

export %inline
iframe : List (Attribute IFrame) -> List HTMLNode -> HTMLNode
iframe = El _

export %inline
img : List (Attribute Img) -> List HTMLNode -> HTMLNode
img = El _

export %inline
input : List (Attribute Tag.Input) -> List HTMLNode -> HTMLNode
input = El _

export %inline
ins : List (Attribute Ins) -> List HTMLNode -> HTMLNode
ins = El _

export %inline
label : List (Attribute Label) -> List HTMLNode -> HTMLNode
label = El _

export %inline
legend : List (Attribute Legend) -> List HTMLNode -> HTMLNode
legend = El _

export %inline
li : List (Attribute Li) -> List HTMLNode -> HTMLNode
li = El _

export %inline
link : List (Attribute Link) -> List HTMLNode -> HTMLNode
link = El _

export %inline
map : List (Attribute Tag.Map) -> List HTMLNode -> HTMLNode
map = El _

export %inline
menu : List (Attribute Menu) -> List HTMLNode -> HTMLNode
menu = El _

export %inline
meta : List (Attribute Meta) -> List HTMLNode -> HTMLNode
meta = El _

export %inline
meter : List (Attribute Meter) -> List HTMLNode -> HTMLNode
meter = El _

export %inline
object : List (Attribute Tag.Object) -> List HTMLNode -> HTMLNode
object = El _

export %inline
ol : List (Attribute Ol) -> List HTMLNode -> HTMLNode
ol = El _

export %inline
optgroup : List (Attribute OptGroup) -> List HTMLNode -> HTMLNode
optgroup = El _

export %inline
option : List (Attribute Option) -> List HTMLNode -> HTMLNode
option = El _

export %inline
output : List (Attribute Output) -> List HTMLNode -> HTMLNode
output = El _

export %inline
p : List (Attribute P) -> List HTMLNode -> HTMLNode
p = El _

export %inline
param : List (Attribute Param) -> List HTMLNode -> HTMLNode
param = El _

export %inline
picture : List (Attribute Picture) -> List HTMLNode -> HTMLNode
picture = El _

export %inline
pre : List (Attribute Pre) -> List HTMLNode -> HTMLNode
pre = El _

export %inline
progress : List (Attribute Progress) -> List HTMLNode -> HTMLNode
progress = El _

export %inline
q : List (Attribute Q) -> List HTMLNode -> HTMLNode
q = El _

export %inline
script : List (Attribute Script) -> List HTMLNode -> HTMLNode
script = El _

export %inline
section : List (Attribute Section) -> List HTMLNode -> HTMLNode
section = El _

export %inline
select : List (Attribute Select) -> List HTMLNode -> HTMLNode
select = El _

export %inline
slot : List (Attribute Slot) -> List HTMLNode -> HTMLNode
slot = El _

export %inline
source : List (Attribute Source) -> List HTMLNode -> HTMLNode
source = El _

export %inline
span : List (Attribute Span) -> List HTMLNode -> HTMLNode
span = El _

export %inline
style : List (Attribute Style) -> List HTMLNode -> HTMLNode
style = El _

export %inline
table : List (Attribute Table) -> List HTMLNode -> HTMLNode
table = El _

export %inline
tbody : List (Attribute Tbody) -> List HTMLNode -> HTMLNode
tbody = El _

export %inline
td : List (Attribute Td) -> List HTMLNode -> HTMLNode
td = El _

export %inline
template : List (Attribute Template) -> List HTMLNode -> HTMLNode
template = El _

export %inline
textarea : List (Attribute TextArea) -> List HTMLNode -> HTMLNode
textarea = El _

export %inline
tfoot : List (Attribute Tfoot) -> List HTMLNode -> HTMLNode
tfoot = El _

export %inline
th : List (Attribute Th) -> List HTMLNode -> HTMLNode
th = El _

export %inline
thead : List (Attribute Thead) -> List HTMLNode -> HTMLNode
thead = El _

export %inline
time : List (Attribute Tag.Time) -> List HTMLNode -> HTMLNode
time = El _

export %inline
title : List (Attribute Title) -> List HTMLNode -> HTMLNode
title = El _

export %inline
tr : List (Attribute Tr) -> List HTMLNode -> HTMLNode
tr = El _

export %inline
track : List (Attribute Track) -> List HTMLNode -> HTMLNode
track = El _

export %inline
ul : List (Attribute Ul) -> List HTMLNode -> HTMLNode
ul = El _

export %inline
video : List (Attribute Video) -> List HTMLNode -> HTMLNode
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
  El {tag} _ as ns  => "<\{tag}\{attrs as}>\{go [<] ns}</\{tag}>"
  Empty             => ""

  where
    go : SnocList String -> List HTMLNode -> String
    go ss (n :: ns) = go (ss :< render n) ns
    go ss []        = concat $ ss <>> []

export
renderMany : List HTMLNode -> String
renderMany = fastConcat . map render
