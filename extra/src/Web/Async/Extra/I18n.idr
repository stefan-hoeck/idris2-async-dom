module Web.Async.Extra.I18n

import public Web.Async.I18n

%default total

public export
interface DOMLocal => ExtraLocal where
  clearTxt              : String
  logTxt                : String

