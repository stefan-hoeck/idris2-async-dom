module Text.HTML.Extra.Class

import IO.Async.Logging
import public Text.CSS.Class

%default total

export
level : LogLevel -> Class
level l = C "cyby-loglvl-\{l}"

||| A class for the async logging element
export %inline
asyncLog : Class
asyncLog = "async-log"

||| A class for hidden elements (CSS: `display: None`)
export %inline
hidden : Class
hidden = "async-dom-hidden"

||| A class for widget separators
export %inline
sep : Class
sep = "async-dom-sep"

||| A class for spacers in flex boxes
export %inline
spacer : Class
spacer = "async-dom-spacer"

||| A UI widget that should be styled similar to a regular button.
export %inline
btn : Class
btn = "async-dom-button"

||| A button with a background image
export %inline
icon : Class
icon = "async-dom-icon"

||| A round icon
export %inline
roundIcon : Class
roundIcon = "async-dom-round-icon"

||| A div-wrapper around a validated DOM element and
||| its validation icon.
export %inline
validatedInput : Class
validatedInput = "async-dom-validated-input"


||| An icon to signal that some mandatory input is missing.
export %inline
iconMissing : Class
iconMissing = "async-dom-icon-missing"

||| An icon to signal that some input is invalid
export %inline
iconError : Class
iconError = "async-dom-icon-error"

||| An icon used to expand a collapsed section in the UI
export %inline
expandIcon : Class
expandIcon = "async-dom-expand-icon"

||| An icon used to remove/delete a piece of data.
export %inline
deleteIcon : Class
deleteIcon = "async-dom-delete-icon"

||| An icon used to confirm an action.
export %inline
okIcon : Class
okIcon = "async-dom-ok-icon"

||| An icon used to add some data.
export %inline
addIcon : Class
addIcon = "async-dom-add-icon"

||| An icon representing a `True` value.
export %inline
trueIcon : Class
trueIcon = "async-dom-true-icon"

||| An icon representing a `False` value.
export %inline
falseIcon : Class
falseIcon = "async-dom-false-icon"
