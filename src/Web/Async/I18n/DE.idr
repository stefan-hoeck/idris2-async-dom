module Web.Async.I18n.DE

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

  export
  [DOMEN] DOMLocal using JSEN where
    editRes Missing       = "mandatory field"
    editRes (Invalid err) = err
    editRes (Valid val)   = ""
    fileStr               = "file"

    logRes nm Missing     = debug "missing \{nm}"
    logRes nm (Invalid x) = debug "invalid \{nm}: \{x}"
    logRes nm (Valid x)   = debug "valid \{nm}: \{x}"


    logSelect Nothing           = debug "no value selected"
    logSelect (Just $ SE n s _) = debug "value selected: '\{s}' (index: \{show n})"

    ldebug s              = debug s
    ltrace s              = trace s

    logAbort          = debug "aborting stream via kill switch"
    logEnded          = debug "stream ended"
    logFormField      = logRes' . interpolate
    logFormFieldN e r = logRes' "field \{show e}" r
    logFormRes        = logRes' "form data"
    logInput s        = debug $ "text input: '\{s}'"
    logReplaced       = debug "replaced bound editor"
    logSwitch         = debug "switching bound editor"
    logSwitchStopped  = debug "stopped bound editor"
