module Web.Async.Example.Selector

import Derive.Finite
import Text.HTML.Select
import Web.Async.Example.Balls
import Web.Async.Example.CSS
import Web.Async.Example.CSS.Core
import Web.Async.Example.MathGame
import Web.Async.Example.Performance
import Web.Async.Example.Request
import Web.Async.Example.Reset
import Web.Async.Example.Util

%default total
%language ElabReflection

data App = Reset | Perf | Balls | Req | Math  -- Fract

%runElab derive "App" [Show,Eq,Finite]

appName : App -> String
appName Reset = "Counting Clicks"
appName Req   = "Processing HTTP Requests"
appName Perf  = "Performance"
appName Balls = "Bouncing Balls"
-- appName Fract = "Fractals"
appName Math  = "Math Game"

content : Sink App => App -> HTMLNode
content ini =
  div [ class contentList ]
      [ div [class pageTitle] ["dom-mvc: Examples"]
      , div [class contentHeader]
          [ label [class widgetLabel] ["Choose an Example"]
          , selectFromList values (Just ini) appName id
              [classes [widget, selectIn, exampleSelector]]
          ]
      , div [Id exampleDiv] []
      ]

prog : App -> JSStream Void
prog Reset = Reset.run
prog Perf  = Performance.run
prog Balls = Balls.run
prog Req   = Request.run
prog Math  = MathGame.run

export covering
ui : IO ()
ui =
  runJS $ do
    style appStyle rules
    app   <- signal Reset
    child contentDiv (content Reset)
    pullErr $ discrete app |> switchMap prog
