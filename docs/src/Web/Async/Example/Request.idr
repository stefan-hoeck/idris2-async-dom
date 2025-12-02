module Web.Async.Example.Request

import Derive.Prelude
import JSON.Simple.Derive
import Web.Async.Example.CSS.Requests
import Web.Async.Example.Util
import Web.Async.HTTP

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
  Got      : Either HTTPError Quote -> ReqEv

content : Sink ReqEv => HTMLNode
content =
  div [ class requestContent ]
    [ button [onClick GetQuote, classes [quoteBtn, widget, btn]] ["get quote"]
    , blockquote [ Id quote ] [ ]
    , p [ Id quoteInfo ] []
    ]

printError : HTTPError -> String
printError Timeout = "connection timed out"
printError NetworkError = "error when connecting to server"
printError (BadStatus m) = "server responded with bad status code: \{show m}"
printError (JSONError str x) =
  """
  Error when decoding JSON string: \{str}

  \{x}
  """

dispResult : Either HTTPError Quote -> HTMLNodes
dispResult (Left x)  = [ div [class requestError ] [ Text $ printError x] ]
dispResult (Right q) =
  [ Text "— "
  , div [] [ Text q.source ]
  , Text " by \{q.author} (\{show q.year})"
  ]

export
run : JSStream Void
run =
  mvcAct ReqInit () $ \e,_ => case e of
    ReqInit  => child exampleDiv content
    GetQuote =>
      ignore $ lift1 $ getJSON "https://elm-lang.org/api/random-quotes" Got
    Got x    => do
      children quoteInfo (dispResult x)
      text quote $ either (const "") quote x
