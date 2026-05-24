module Web.Async.I18n

import IO.Async.JS
import Text.HTML.Select
import Web.Async.Widget.Types
import public HTTP.API.Client.I18n

%default total

public export
interface JSLocal => DOMLocal where
  editRes          : EditRes t -> String
  fileStr          : String
  ldebug           : Lazy String -> Async JS es ()
  ltrace           : Lazy String -> Async JS es ()

  logEnded         : Async JS es ()
  logFormField     : Interpolation t => t -> EditRes s -> Async JS es ()
  logFormFieldN    : Nat -> EditRes s -> Async JS es ()
  logFormRes       : EditRes s -> Async JS es ()
  logInput         : String -> Async JS es ()
  logRemove        : Async JS es ()
  logReplaced      : Async JS es ()
  logRes           : Interpolation t => String -> EditRes t -> Async JS es ()
  logSelect        : Maybe (SelectEv t) -> Async JS es ()
  logSwitch        : Async JS es ()
  logSwitchStopped : Async JS es ()
