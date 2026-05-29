module Text.HTML.Extra.Attribute

import public Text.HTML.Attribute

%default total

export %inline
active : Bool -> Attribute t
active = Bool "data-active"
