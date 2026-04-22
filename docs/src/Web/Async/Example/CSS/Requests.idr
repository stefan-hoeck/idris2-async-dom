module Web.Async.Example.CSS.Requests

import Data.Vect
import Derive.Prelude
import public Web.Async.Example.CSS.Core

%language ElabReflection
%default total

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

export
quote : Ref Blockquote
quote = Id "request-quote"

export
quoteInfo : Ref Tag.P
quoteInfo = Id "request-quote-info"

--------------------------------------------------------------------------------
--          Classes
--------------------------------------------------------------------------------

export
requestContent : Class
requestContent = "request-content"

export
requestError : Class
requestError = "request-error"

export
quoteBtn : Class
quoteBtn = "request-quote-btn"

--------------------------------------------------------------------------------
--          Rules
--------------------------------------------------------------------------------

data Tag = Btn | Quot | Info | Dot

%runElab derive "Tag" [Show,Eq]

export
css : List (Rule 1)
css =
  [ Media "min-width: 300px"
      [ class requestContent
          [ Display             $ Area
              [MinContent, MaxContent, MaxContent]
              [px 200, px 400]
              [ [Btn, Dot]
              , [Quot, Quot]
              , [Info, Info]
              ]
          , columnGap 10.px
          , rowGap 10.px
          , padding $ VH 20.px 10.px
          ]
      ]

  , ref quote     [gridArea Quot]

  , ref quoteInfo [gridArea Info, textAlign End]

  , class quoteBtn [gridArea Btn]
  ]
