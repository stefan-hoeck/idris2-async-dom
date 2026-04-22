module Web.Async.Example.CSS.Balls

import Data.Vect
import Derive.Prelude
import public Web.Async.Example.CSS.Core

%language ElabReflection
%default total

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

export
out : Ref Canvas
out = Id "balls_out"

export
btnRun : Ref Tag.Button
btnRun = Id "balls_run"

export
txtCount : Ref Tag.Input
txtCount = Id "balls_numballs"

export
log : Ref Div
log = Id "balls_log"

--------------------------------------------------------------------------------
--          Rules
--------------------------------------------------------------------------------

export
ballsContent : Class
ballsContent = "balls_content"

export
lblCount : Class
lblCount = "balls_lblcount"

data Tag = LNum | INum | BRun | LFPS | Anim | Dot

%runElab derive "Tag" [Show,Eq]

export
css : List (Rule 1)
css =
  [ Media "min-width: 300px"
      [ class ballsContent
          [ display             $ Area
              (replicate 4 MinContent)
              [MaxContent, MaxContent]
              [ [LNum, INum]
              , [Dot,  BRun]
              , [LFPS, LFPS]
              , [Anim, Anim]
              ]

          , columnGap 10.px
          , rowGap 10.px
          , padding $ VH 20.px 10.px
          ]
      ]

  , Media "min-width: 800px"
      [ class ballsContent
          [ display             $ Area
              (replicate 4 MinContent)
              [MaxContent, MaxContent, fr 1]
              [ [LNum, INum, Anim]
              , [Dot,  BRun, Anim]
              , [LFPS, LFPS, Anim]
              , [Dot,  Dot,  Anim]
              ]

          , columnGap 10.px
          , rowGap 10.px
          , padding $ VH 20.px 10.px
          ]
      ]

  , class lblCount [ gridArea LNum ]

  , ref txtCount
      [ gridArea        INum
      , textAlign       End
      ]

  , ref btnRun [ gridArea BRun ]

  , ref log [ gridArea LFPS ]

  , ref out
      [ justifySelf Center
      , gridArea Anim
      , maxWidth 500.px
      , width 500.px
      ]
  ]
