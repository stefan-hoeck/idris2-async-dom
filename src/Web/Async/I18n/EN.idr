module Web.Async.I18n.EN

import HTTP.API.Client.I18n.EN
import IO.Async.JS
import IO.Async.Logging
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

    logRes nm Missing     = trace "missing \{nm}"
    logRes nm (Invalid x) = trace "invalid \{nm}: \{x}"
    logRes nm (Valid x)   = trace "valid \{nm}: \{x}"

    logFormField fld Missing     = trace "missing \{fld}"
    logFormField fld (Invalid x) = trace "invalid \{fld}: \{x}"
    logFormField fld (Valid x)   = trace "valid \{fld}"

    logFormRes Missing     = trace "missing form data"
    logFormRes (Invalid x) = trace "invalid form data: \{x}"
    logFormRes (Valid x)   = trace "valid form data"

    logSelect Missing     = trace "no value selected"
    logSelect (Invalid x) = trace "invalid value selected: \{x}"
    logSelect (Valid x)   = trace "value selected: \{x}"

    ldebug s              = debug s
    ltrace s              = trace s
    logInput s            = trace $ "text input: '\{s}'"

