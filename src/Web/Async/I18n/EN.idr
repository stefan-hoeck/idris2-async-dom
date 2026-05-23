module Web.Async.I18n.EN

import HTTP.API.Client.I18n.EN
import IO.Async.JS
import IO.Async.Logging
import Text.HTML.Select
import Web.Async.Widget.Types
import public Web.Async.I18n

%default total

parameters {auto log : Logger JS}

  logRes' : String -> EditRes x -> Async JS es ()
  logRes' nm Missing     = debug "missing \{nm}"
  logRes' nm (Invalid x) = debug "invalid \{nm}: \{x}"
  logRes' nm (Valid x)   = debug "valid \{nm}"

  logN : Nat -> EditRes s -> Async JS es ()
  logN e r = logRes' "field \{show e}" r

  export
  DOMLocal where
    editRes Missing       = "mandatory field"
    editRes (Invalid err) = err
    editRes (Valid val)   = ""
    fileStr               = "file"

    logRes nm Missing     = debug "missing \{nm}"
    logRes nm (Invalid x) = debug "invalid \{nm}: \{x}"
    logRes nm (Valid x)   = debug "valid \{nm}: \{x}"

    logEnded  = debug "stream ended"
    logRemove = debug "element removed"

    logFormField = logRes' . interpolate

    logFormRes = logRes' "form data"

    logFormFieldN = ?foobar

    logSelect Nothing           = debug "no value selected"
    logSelect (Just $ SE n s _) = debug "value selected: '\{s}' (index: \{show n})"

    ldebug s              = debug s
    ltrace s              = trace s
    logInput s            = debug $ "text input: '\{s}'"
