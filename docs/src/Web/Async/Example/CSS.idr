module Web.Async.Example.CSS

import Data.String
import Web.Async.Example.CSS.Balls
import Web.Async.Example.CSS.Core
import Web.Async.Example.CSS.Fractals
import Web.Async.Example.CSS.MathGame
import Web.Async.Example.CSS.Performance
import Web.Async.Example.CSS.Reset
import Web.Async.Example.CSS.Requests

%default total

export
rules : List (Rule 1)
rules =
     coreCSS
  ++ Balls.css
  ++ Fractals.css
  ++ MathGame.css
  ++ Performance.css
  ++ Reset.css
  ++ Requests.css
