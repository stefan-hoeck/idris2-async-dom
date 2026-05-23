module Web.Async.I18n.EN

import HTTP.API.Client.I18n.EN
import IO.Async.JS
import IO.Async.Logging
import Text.HTML.Select
import Web.Async.Widget.Types
import public Web.Async.I18n

%default total

parameters {auto log : Logger JS}

  export
  DOMLocal where
    editRes Missing       = "mandatory field"
    editRes (Invalid err) = err
    editRes (Valid val)   = ""
    fileStr               = "file"

    logRes nm Missing     = debug "missing \{nm}"
    logRes nm (Invalid x) = debug "invalid \{nm}: \{x}"
    logRes nm (Valid x)   = debug "valid \{nm}: \{x}"

    logFormField fld Missing     = debug "missing \{fld}"
    logFormField fld (Invalid x) = debug "invalid \{fld}: \{x}"
    logFormField fld (Valid x)   = debug "valid \{fld}"

    logFormRes Missing     = debug "missing form data"
    logFormRes (Invalid x) = debug "invalid form data: \{x}"
    logFormRes (Valid x)   = debug "valid form data"

    logSelect Nothing           = debug "no value selected"
    logSelect (Just $ SE n s _) = debug "value selected: '\{s}' (index: \{show n})"

    ldebug s              = debug s
    ltrace s              = trace s
    logInput s            = debug $ "text input: '\{s}'"
