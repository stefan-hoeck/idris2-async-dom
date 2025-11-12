module Text.HTML.Attribute

import Derive.Prelude
import Data.Linear.Token
import Data.List
import Data.Maybe
import Data.String
import Text.HTML.Event
import Text.HTML.Ref
import Text.HTML.Tag

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- Sink
--------------------------------------------------------------------------------

public export
record Sink a where
  [noHints]
  constructor S
  sink1 : a -> IO1 ()

export %inline
cmap : (b -> a) -> Sink a -> Sink b
cmap f (S g) = S (g . f)

export %inline
sink : HasIO io => (s : Sink a) => a -> io ()
sink = runIO . s.sink1

export %inline
sinkAs : HasIO io => (0 a : Type) -> (s : Sink a) => a -> io ()
sinkAs a = sink

--------------------------------------------------------------------------------
-- Attribute
--------------------------------------------------------------------------------

public export
data Dir = LTR | RTL

export
Show Dir where
  show LTR = "ltr"
  show RTL = "rtl"

public export
data LoadType = Lzy | Eager

export
Show LoadType where
  show Lzy   = "lazy"
  show Eager = "eager"

||| Enum representing different types of input elements
public export
data InputType =
    Button
  | CheckBox
  | Color
  | Date
  | DateTime
  | Email
  | File
  | Image
  | Month
  | Number
  | Password
  | Radio
  | Range
  | Tel
  | Text
  | Time
  | URL
  | Week

export
Show InputType where
  show Button   = "button"
  show CheckBox = "checkbox"
  show Color    = "color"
  show Date     = "date"
  show DateTime = "datetime-local"
  show Email    = "email"
  show File     = "file"
  show Image    = "image"
  show Month    = "month"
  show Number   = "number"
  show Password = "password"
  show Radio    = "radio"
  show Range    = "range"
  show Tel      = "tel"
  show Text     = "text"
  show Time     = "time"
  show URL      = "url"
  show Week     = "week"

||| A CSS class
public export
record Class where
  constructor C
  value : String

%runElab derive "Class" [Show,Eq,Ord,FromString]

public export
0 Classes : Type
Classes = List Class

||| An attribute indexed by the `HTMLTag` used for the element
||| in question.
|||
||| This allows us to make sure we don't use invalid `Ref`s (which can
||| be later used to retrieve an element from the DOM) in a HTML node.
public export
data Attribute : {0 k : Type} -> (t : k) -> Type where
  Id     : {0 t : HTMLTag s} -> Ref t -> Attribute t
  Str    : (name : String) -> (value : String) -> Attribute t
  Bool   : (name : String) -> (value : Bool) -> Attribute t
  Event_ :
       (preventDefault, stopPropagation : Bool)
    -> {auto sink : Sink event}
    -> DOMEvent event
    -> Attribute t
  Empty  : Attribute t

||| Optional attribute that is set to `Empty` if the given `Bool` is `False`
export
attrIf : Bool -> Lazy (Attribute t) -> Attribute t
attrIf True n  = n
attrIf False _ = Empty

public export
Attributes : {0 k : _} -> (t : k) -> Type
Attributes t = List (Attribute t)

export %inline
Event : Sink ev => DOMEvent ev -> Attribute t
Event = Event_ False False

export
displayAttribute : Attribute t -> Maybe String
displayAttribute (Id (Id va))   = Just #"id="\#{va}""#
displayAttribute (Str nm va)    = Just #"\#{nm}="\#{va}""#
displayAttribute (Bool nm True) = Just nm
displayAttribute (Bool _ False) = Nothing
displayAttribute (Event_ _ _ _) = Nothing
displayAttribute Empty          = Nothing

export
displayAttributes : Attributes t -> String
displayAttributes = fastConcat . intersperse " " . mapMaybe displayAttribute

export
dispAttr : String -> (a -> String) -> a -> Attribute t
dispAttr nm f =  Str nm . f

export
showAttr : Show a => String -> a -> Attribute t
showAttr nm = dispAttr nm show

export %inline
accesskey : String -> Attribute t
accesskey = Str "accesskey"

export %inline
action : String -> Attribute t
action = Str "action"

export %inline
alt : String -> Attribute t
alt = Str "alt"

export %inline
autocapitalize : Bool -> Attribute t
autocapitalize = Bool "autocapitalize"

export %inline
autocomplete : Bool -> Attribute t
autocomplete = Bool "autocomplete"

export %inline
autofocus : Bool -> Attribute t
autofocus = Bool "autofocus"

export %inline
autoplay : Bool -> Attribute t
autoplay = Bool "autoplay"

export %inline
checked : Bool -> Attribute t
checked = Bool "checked"

export %inline
cite : String -> Attribute t
cite = Str "cite"

export %inline
class : Class -> Attribute t
class = Str "class" . value

export %inline
classes : Classes -> Attribute t
classes = dispAttr "class" (fastConcat . intersperse " " . map value)

export %inline
cols : Bits32 -> Attribute t
cols = showAttr "cols"

export %inline
colspan : Bits32 -> Attribute t
colspan = showAttr "colspan"

export %inline
contenteditable : Bool -> Attribute t
contenteditable = Bool "contenteditable"

export %inline
controls : Bool -> Attribute t
controls = Bool "controls"

export %inline
data_ : String -> Attribute t
data_ = Str "data"

export %inline
dir : Dir -> Attribute t
dir = showAttr "dir"

export %inline
disabled : Bool -> Attribute t
disabled = Bool "disabled"

export %inline
download : String -> Attribute t
download = Str "download"

export %inline
draggable : Bool -> Attribute t
draggable = Bool "draggable"

export %inline
for : String -> Attribute t
for = Str "for"

export %inline
form : String -> Attribute t
form = Str "form"

export %inline
height : Bits32 -> Attribute t
height = showAttr "height"

export %inline
hidden : Bool -> Attribute t
hidden = Bool "hidden"

export %inline
href : String -> Attribute t
href = Str "href"

export %inline
hreflang : String -> Attribute t
hreflang = Str "hreflang"

export %inline
id : String -> Attribute t
id = Str "id"

export %inline
label : String -> Attribute t
label = Str "label"

export %inline
lang : String -> Attribute t
lang = Str "lang"

export %inline
loading : LoadType -> Attribute t
loading = showAttr "loading"

export %inline
list : String -> Attribute t
list = Str "list"

export %inline
loop : Bool -> Attribute t
loop = Bool "loop"

export %inline
maxlength : Bits32 -> Attribute t
maxlength = showAttr "maxlength"

export %inline
minlength : Bits32 -> Attribute t
minlength = showAttr "minlength"

export %inline
multiple : Bool -> Attribute t
multiple = Bool "multiple"

export %inline
muted : Bool -> Attribute t
muted = Bool "muted"

export %inline
name : String -> Attribute t
name = Str "name"

export %inline
placeholder : String -> Attribute t
placeholder = Str "placeholder"

export %inline
readonly : Bool -> Attribute t
readonly = Bool "readonly"

export %inline
required : Bool -> Attribute t
required = Bool "required"

export %inline
reverse : Bool -> Attribute t
reverse = Bool "reverse"

export %inline
rows : Bits32 -> Attribute t
rows = showAttr "rows"

export %inline
rowspan : Bits32 -> Attribute t
rowspan = showAttr "rowspan"

export %inline
selected : Bool -> Attribute t
selected = Bool "selected"

export %inline
spellcheck : Bool -> Attribute t
spellcheck = Bool "spellcheck"

export %inline
src : String -> Attribute t
src = Str "src"

export %inline
style : String -> Attribute t
style = Str "style"

export %inline
tabindex : Int32 -> Attribute t
tabindex = showAttr "tabindex"

export %inline
target : String -> Attribute t
target = Str "target"

export %inline
title : String -> Attribute t
title = Str "title"

export %inline
type : InputType -> Attribute t
type = showAttr "type"

export %inline
value : String -> Attribute t
value = Str "value"

export %inline
width : Bits32 -> Attribute t
width = showAttr "width"

export %inline
wrap : Bool -> Attribute t
wrap = Bool "wrap"

--------------------------------------------------------------------------------
--          Events
--------------------------------------------------------------------------------

parameters {auto sink : Sink ev}

  export %inline
  click : (MouseInfo -> Maybe ev) -> Attribute t
  click = Event . Click

  export %inline
  onClick : ev -> Attribute t
  onClick = click . const . Just

  export
  onLeftClick : ev -> Attribute t
  onLeftClick va = click $ \mi => toMaybe (mi.button == 0) va

  export
  onRightClick : ev -> Attribute t
  onRightClick va = click $ \mi => toMaybe (mi.button == 2) va

  export
  onMiddleClick : ev -> Attribute t
  onMiddleClick va = click $ \mi => toMaybe (mi.button == 1) va

  export %inline
  dblClick : (MouseInfo -> Maybe ev) -> Attribute t
  dblClick = Event . DblClick

  export %inline
  onDblClick : ev -> Attribute t
  onDblClick = dblClick . const . Just

  export %inline
  onMouseEnter : ev -> Attribute t
  onMouseEnter = Event . MouseEnter . const . Just

  export %inline
  onMouseLeave : ev -> Attribute t
  onMouseLeave = Event . MouseLeave . const . Just

  export %inline
  onMouseOver : ev -> Attribute t
  onMouseOver = Event . MouseOver . const . Just

  export %inline
  onMouseOut : ev -> Attribute t
  onMouseOut = Event . MouseOut . const . Just

  export
  onResize : (Rect -> ev) -> Attribute t
  onResize f = Event . Resize $ Just . f

  export
  onChange : (String -> ev) -> Attribute t
  onChange f = Event . Change $ Just . f . value

  export
  onChangeMaybe : (String -> Maybe ev) -> Attribute t
  onChangeMaybe f = Event . Change $ f . value

  export
  onChecked : (Bool -> ev) -> Attribute t
  onChecked f = Event . Change $ Just . f . checked

  export
  onInput : (String -> ev) -> Attribute t
  onInput f = Event . Input $ Just . f . value

  export
  onScroll : (ScrollInfo -> ev) -> Attribute t
  onScroll f = Event . Scroll $ Just . f

  export
  onEnterDown : ev -> Attribute t
  onEnterDown va = Event . KeyDown $ \k => toMaybe (k.key == "Enter") va

  export
  onEscDown : ev -> Attribute t
  onEscDown va = Event . KeyDown $ \k => toMaybe (k.key == "Escape") va

  export
  onKeyUp : (KeyInfo -> ev) -> Attribute t
  onKeyUp f = Event . KeyUp $ Just . f

  export
  onBlur : ev -> Attribute t
  onBlur = Event . Blur

  export
  onFocus : ev -> Attribute t
  onFocus = Event . Focus
