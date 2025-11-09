module Web.Async.Example.Main

import IO.Async.JS
import Web.Async.Example.Selector

%default total

covering
main : IO ()
main = app $ handle [putStrLn . dispErr] ui
