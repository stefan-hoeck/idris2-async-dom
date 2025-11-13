||| Reocurring fragments from the example applications
module Web.Async.Example.Util

import public Web.Async
import Web.Async.Example.CSS.Core

%default total

export
lbl : (text: String) -> (class : Class) -> HTMLNode
lbl txt cl = label [classes [widgetLabel, cl]] [Text txt]
