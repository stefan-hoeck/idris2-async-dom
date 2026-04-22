module Web.Async.Example.CSS.Fractals

import Data.Vect
import Derive.Prelude
import public Web.Async.Example.CSS.Core

%language ElabReflection
%default total

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

export
out : Ref Div
out = Id "fractals_out"

export
btnRun : Ref Tag.Button
btnRun = Id "fractals_run"

export
txtIter : Ref Tag.Input
txtIter = Id "fractals_iterations"

export
txtRedraw : Ref Tag.Input
txtRedraw = Id "fractals_redrawdelay"

--------------------------------------------------------------------------------
--          Classes
--------------------------------------------------------------------------------

export
fractalContent : Class
fractalContent = "fractals_content"

export
lblIter : Class
lblIter = "fractals_lbliter"

export
lblDelay : Class
lblDelay = "fractals_lbldelay"

--------------------------------------------------------------------------------
--          Rules
--------------------------------------------------------------------------------

data Tag = LIter | IIter | LDel | IDel | BRun | Fract | Dot

%runElab derive "Tag" [Show,Eq]

export
css : List (Rule 1)
css =
  [ Media "min-width: 300px"
      [ class fractalContent
          [ Display             $ Area
              (replicate 4 MinContent)
              [MaxContent, MaxContent]
              [ [LIter, IIter]
              , [LDel,  IDel ]
              , [Dot,   BRun ]
              , [Fract, Fract]
              ]
          , columnGap 10.px
          , rowGap 10.px
          , padding $ VH 20.px 10.px
          ]
      ]

  , Media "min-width: 800px"
      [ class fractalContent
          [ Display             $ Area
              (replicate 4 MinContent)
              [MaxContent, MaxContent, fr 1]
              [ [LIter, IIter, Fract]
              , [LDel,  IDel,  Fract]
              , [Dot,   BRun,  Fract]
              , [Dot,   Dot,   Fract]
              ]
          , columnGap 10.px
          , rowGap 10.px
          , padding $ VH 20.px 10.px
          ]
      ]
  , class lblIter [ gridArea LIter ]

  , ref txtIter
      [ gridArea        IIter
      , textAlign       End
      ]

  , class lblDelay [ gridArea LDel ]

  , ref txtRedraw
      [ gridArea        IDel
      , textAlign       End
      ]

  , ref btnRun [ gridArea BRun ]

  , ref out
      [ justifySelf     Center
      , gridArea        Fract
      , borderStyle     $ Left Solid
      , borderWidth     $ Left 2.px
      , borderColor     $ Left base80
      , maxWidth 500.px
      , width 500.px
      ]
  ]
