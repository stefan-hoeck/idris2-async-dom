module Web.Async.Example.Request

import Derive.Prelude
import HTTP.API.Client
import HTTP.I18n
import JSON.Simple.Derive
import Web.Async.Example.CSS.Requests
import Web.Async.Example.Util

%default total
%language ElabReflection

record Quote where
  constructor Q
  quote  : String
  source : String
  author : String
  year   : Bits16

%runElab derive "Quote" [Show,Eq,FromJSON,ToJSON]

data ReqEv : Type where
  ReqInit  : ReqEv
  GetQuote : ReqEv

content : Sink ReqEv => HTMLNode
content =
  div [ class requestContent ]
    [ button [onClick GetQuote, classes [quoteBtn, widget, btn]] ["get quote"]
    , blockquote [ Id quote ] [ ]
    , p [ Id quoteInfo ] []
    ]

printError : HTTPLocal => HTTPError -> String
printError Timeout = "connection timed out"
printError NetworkError = "error when connecting to server"
printError (DecError st s) = "server responded with status code: \{show st} (\{s})"
printError (ReqError e) = interpolate e

dispResult : HTTPLocal => Result [HTTPError] Quote -> HTMLNodes
dispResult (Left $ Here x)  = [ div [class requestError ] [ Text $ printError x] ]
dispResult (Right q) =
  [ Text "— "
  , div [] [ Text q.source ]
  , Text " by \{q.author} (\{show q.year})"
  ]

Quotes : HList [ReqPath,ReqMethod]
Quotes =
  [path "https" "elm-lang.org" ["api","random-quotes"], Get [JSON] Quote]

export
run : HTTPLocal => JSStream Void
run =
  mvcAct ReqInit () $ \e,_ => case e of
    ReqInit  => child exampleDiv content
    GetQuote => Prelude.do
      res <- attempt (requestJSONEndpoint Quotes [] Quote)
      children quoteInfo (dispResult res)
      text quote $ either (const "") quote res
