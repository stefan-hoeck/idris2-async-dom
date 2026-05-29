module Text.HTML.Extra.Node

import Data.Linear.Sink
import Data.List

import public Text.HTML
import public Text.HTML.DomID
import public Text.HTML.Extra.Class
import public Text.HTML.Select

%default total

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

||| ID of an element to which logging messages can be sent.
export
AsyncLog : DomID
AsyncLog = "async-dom-log"

export %inline
nodeSep : HTMLNode
nodeSep = div [class sep] []

export %inline
spacer : HTMLNode
spacer = div [class spacer] []

export %inline
separate : HTMLNodes -> HTMLNodes
separate = intersperse nodeSep

--------------------------------------------------------------------------------
-- Icons
--------------------------------------------------------------------------------

||| An icon showing a warning triangle with an exclamation mark
export
iwarn : HTMLNode

export
iok : HTMLNode

export
iadd : HTMLNode

export
icancel : HTMLNode

export
idelete : HTMLNode

export
iexpanded : HTMLNode

export
icollapsed : HTMLNode

export
ireload : HTMLNode

export
isortDec : HTMLNode

export
isortInc : HTMLNode

export
inoSort : HTMLNode

--------------------------------------------------------------------------------
-- Widgets
--------------------------------------------------------------------------------

||| Creates a text label for a probably editable field
|||
||| @ uid   : ID used in "for" attribute
||| @ txt   : actual textual content
export
lbl : DomID -> String -> HTMLNode
lbl uiid txt = label [forID uiid] [Text txt]

||| A clickable button in the UI firing the given event on a left click.
export %inline
btn : Sink e => e -> String -> List (Attribute Tag.Button) -> HTMLNode
btn ev txt as = button (onClick ev :: as) [Text txt]

||| A clickable button in the UI firing the given event on a left click.
export %inline
icn :
     {auto snk : Sink e}
  -> {default icon cl : Class}
  -> HTMLNode
  -> e
  -> List (Attribute Tag.Button)
  -> HTMLNode
icn n ev as = button (class cl :: onClick ev :: as) [n]

--------------------------------------------------------------------------------
--          Editing
--------------------------------------------------------------------------------

export %inline
deleteNode : Sink e => e -> HTMLNode
deleteNode ev = icn {cl = deleteIcon} idelete ev []

export %inline
addNode : Sink e => e -> HTMLNode
addNode ev = icn {cl = addIcon} iadd ev []

||| An `<input>` element of the given class.
export %inline
inp : Sink e => (String -> e) -> List (Attribute Tag.Input) -> HTMLNode
inp f as = input (onInput f :: as)

||| A select element displaying the values of type `v`
||| shown in the given list.
|||
||| It fires events of type `t`, and uses two functions, one for
||| converting elements to events and one for displaying elements.
export %inline
sel : Sink t => Eq v => (v -> t) -> (v -> String) -> List v -> Maybe v -> HTMLNode
sel f g vs i = selectFromListBy' vs ((i ==) . Just) g f []

--------------------------------------------------------------------------------
-- Icons SVGs
--------------------------------------------------------------------------------

iwarn = Raw "<svg viewBox='0 0 6.61 6.61'><g transform='matrix(.902 0 0 .902 .319 .685)' fill='none' stroke-width='.419'><g stroke='currentcolor' stroke-linecap='round' stroke-linejoin='round'><path transform='translate(1.35,3)' d='m4.86 2.42-2.9-2e-7-2.9 1e-7 2.9-5.03 1.45 2.51z' stroke-width='.419'/><path d='m3.31 2.05v1.71' stroke-width='.629'/><path d='m3.31 4.63v.0865' stroke-width='.629'/></g></g></svg>"
iok = Raw "<svg viewBox='0 0 6.61 6.61'><path d='m1.32 3.97 1.32 1.06 2.65-3.7-2.74 3-1.23-.351' fill='currentcolor' fill-rule='evenodd' stroke='currentcolor' stroke-width='.0794'/></svg>"
iadd = Raw "<svg viewBox='0 0 6.61 6.61'><path transform='scale(.265)' d='m11 5v6h-6v3h6v6h3v-6h6v-3h-6v-6h-3z' fill='currentcolor' stroke='currentcolor' stroke-width='.3'/></svg>"
icancel = Raw "<svg viewBox='0 0 6.61 6.61'><path d='m4.43 1.62-1.12 1.12-1.12-1.12-.561.561 1.12 1.12-1.12 1.12.561.561 1.12-1.12 1.12 1.12.561-.561-1.12-1.12 1.12-1.12z' fill='currentcolor' stroke='currentcolor' stroke-width='.0794'/></svg>"
idelete = icancel
iexpanded = Raw "<svg viewBox='0 0 6.61 6.61'><path d='m5.12 1.9-1.81 3.22-1.81-3.22 1.81 1.41z' fill='currentcolor' fill-rule='evenodd' stroke='currentcolor' stroke-width='.0794'/></svg>"
icollapsed = Raw "<svg viewBox='0 0 6.61 6.61'><path d='m1.9 1.5 3.22 1.81-3.22 1.81 1.41-1.81z' fill='currentcolor' fill-rule='evenodd' stroke='currentcolor' stroke-width='.0794'/></svg>"
ireload = Raw "<svg viewBox='0 0 6.61 6.61'><g transform='rotate(-30 3.23 3.6)' fill='currentcolor'><path transform='scale(.265)' d='m12.5 2a10.5 10.5 0 00-10.5 10.5 10.5 10.5 0 0010.5 10.5v-3a7.5 7.5 0 01-7.5-7.5 7.5 7.5 0 017.5-7.5 7.5 7.5 0 017.5 7.5h3a10.5 10.5 0 00-10.5-10.5z'/><path d='m4.51 3.11h2.44l-1.22 2.19z' fill-rule='evenodd' stroke-width='1.84'/></g></svg>"
isortDec = Raw "<svg viewBox='0 0 6.61 6.61'><path d='m5.12 3.7-1.81 2.12-1.81-2.12z' fill='currentcolor' fill-rule='evenodd' stroke='currentcolor' stroke-width='.0794'/><path d='m5.12 2.91-1.81-2.12-1.81 2.12z' fill='none' fill-rule='evenodd' stroke='currentcolor' stroke-width='.0794'/></svg>"
isortInc = Raw "<svg viewBox='0 0 6.61 6.61'><path d='m5.12 3.7-1.81 2.12-1.81-2.12z' fill='none' fill-rule='evenodd' stroke='currentcolor' stroke-width='.0794'/><path d='m5.12 2.91-1.81-2.12-1.81 2.12z' fill='currentcolor' fill-rule='evenodd' stroke='currentcolor' stroke-width='.0794'/></svg>"
inoSort = Raw "<svg viewBox='0 0 6.61 6.61'><path d='m5.12 3.7-1.81 2.12-1.81-2.12z' fill='none' fill-rule='evenodd' stroke='currentcolor' stroke-width='.0794'/><path d='m5.12 2.91-1.81-2.12-1.81 2.12z' fill='none' fill-rule='evenodd' stroke='currentcolor' stroke-width='.0794'/></svg>"
