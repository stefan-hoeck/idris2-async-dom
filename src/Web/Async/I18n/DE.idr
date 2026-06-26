module Web.Async.I18n.DE

import HTTP.API.Client.I18n.DE
import IO.Async.JS
import IO.Async.Logging
import Text.HTML.Select
import Web.Async.Widget.Types
import public Web.Async.I18n

%default total

parameters {auto log : Logger JS}

  logRes' : String -> EditRes x -> Async JS es ()
  logRes' nm Missing     = debug "Fehlend \{nm}"
  logRes' nm (Invalid x) = debug "Ungültig \{nm}: \{x}"
  logRes' nm (Valid x)   = debug "Gültig \{nm}"

  export
  [DOMDE] DOMLocal using JSDE where
    editRes Missing       = "Pflichtfeld"
    editRes (Invalid err) = err
    editRes (Valid val)   = ""
    fileStr               = "Datei"

    logRes nm Missing     = debug "Fehlend \{nm}"
    logRes nm (Invalid x) = debug "Ungültig \{nm}: \{x}"
    logRes nm (Valid x)   = debug "Gültig \{nm}: \{x}"


    logSelect Nothing           = debug "Kein Wert ausgewählt"
    logSelect (Just $ SE n s _) = debug "Ausgewählter Wert: '\{s}' (Index: \{show n})"

    ldebug s              = debug s
    ltrace s              = trace s

    logAbort          = debug "Der Datenstrom wurde über den Kill-Switch abgebrochen."
    logEnded          = debug "Der Datenstrom endete"
    logFormField      = logRes' . interpolate
    logFormFieldN e r = logRes' "field \{show e}" r
    logFormRes        = logRes' "form data"
    logInput s        = debug $ "text input: '\{s}'"
    logReplaced       = debug "replaced bound editor"
    logSwitch         = debug "switching bound editor"
    logSwitchStopped  = debug "stopped bound editor"
