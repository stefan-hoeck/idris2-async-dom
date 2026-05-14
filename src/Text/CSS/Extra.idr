module Text.CSS.Extra

import public Text.HTML
import public Text.HTML.DomID
import public Text.CSS

%default total

--------------------------------------------------------------------------------
-- Selectors
--------------------------------------------------------------------------------

export
attr : (t -> Attribute ()) -> t -> Selector
attr f v =
  case f v of
    Str  n v => Attr n $ Equals v
    Bool n _ => Attr n Set
    _        => []

export %inline
boolAttr : (Bool -> Attribute ()) -> Selector
boolAttr f = attr f True

export %inline
domID : DomID -> Declarations -> Rule n
domID = id . value

export %inline
attribute : (t -> Attribute ()) -> t -> Declarations -> Rule n
attribute f = sel . attr f

--------------------------------------------------------------------------------
-- Borders
--------------------------------------------------------------------------------

export %inline
noBorder : Declaration
noBorder = borderStyle (All None)

export
border : BorderWidth -> Color -> Declarations
border w c =
  [borderStyle (All Solid), borderWidth  (All w), borderColor (All c)]

export %inline
border1 : Color -> Declarations
border1 = border 1.px

export %inline
border2 : Color -> Declarations
border2 = border 2.px

export %inline
border3 : Color -> Declarations
border3 = border 3.px

export
borderH : BorderWidth -> Color -> Declarations
borderH w c =
  [ borderStyle (VH None Solid)
  , borderWidth (VH 0.px w)
  , borderColor (VH transparent c)
  ]

export %inline
borderH1 : Color -> Declarations
borderH1 = borderH 1.px

export %inline
borderH2 : Color -> Declarations
borderH2 = borderH 2.px

export %inline
borderH3 : Color -> Declarations
borderH3 = borderH 3.px

export
borderV : BorderWidth -> Color -> Declarations
borderV w c =
  [ borderStyle (VH Solid None)
  , borderWidth (VH w 0.px)
  , borderColor (VH c transparent)
  ]

export %inline
borderV1 : Color -> Declarations
borderV1 = borderV 1.px

export %inline
borderV2 : Color -> Declarations
borderV2 = borderV 2.px

export %inline
borderV3 : Color -> Declarations
borderV3 = borderV 3.px

export
borderT : BorderWidth -> Color -> Declarations
borderT w c =
  [ borderStyle (Top Solid)
  , borderWidth (Top w)
  , borderColor (Top c)
  ]

export %inline
borderT1 : Color -> Declarations
borderT1 = borderT 1.px

export %inline
borderT2 : Color -> Declarations
borderT2 = borderT 2.px

export %inline
borderT3 : Color -> Declarations
borderT3 = borderT 3.px

export
borderR : BorderWidth -> Color -> Declarations
borderR w c =
  [ borderStyle (Right Solid)
  , borderWidth (Right w)
  , borderColor (Right c)
  ]

export %inline
borderR1 : Color -> Declarations
borderR1 = borderR 1.px

export %inline
borderR2 : Color -> Declarations
borderR2 = borderR 2.px

export %inline
borderR3 : Color -> Declarations
borderR3 = borderR 3.px

export
borderB : BorderWidth -> Color -> Declarations
borderB w c =
  [ borderStyle (Bottom Solid)
  , borderWidth (Bottom w)
  , borderColor (Bottom c)
  ]

export %inline
borderB1 : Color -> Declarations
borderB1 = borderB 1.px

export %inline
borderB2 : Color -> Declarations
borderB2 = borderB 2.px

export %inline
borderB3 : Color -> Declarations
borderB3 = borderB 3.px

export
borderL : BorderWidth -> Color -> Declarations
borderL w c =
  [ borderStyle (Left Solid)
  , borderWidth (Left w)
  , borderColor (Left c)
  ]

export %inline
borderL1 : Color -> Declarations
borderL1 = borderL 1.px

export %inline
borderL2 : Color -> Declarations
borderL2 = borderL 2.px

export %inline
borderL3 : Color -> Declarations
borderL3 = borderL 3.px

export
borderHB : BorderWidth -> Color -> Declarations
borderHB w c =
  [ borderStyle (THB None Solid Solid)
  , borderWidth (THB 0.px w w)
  , borderColor (THB transparent c c)
  ]

export %inline
borderHB1 : Color -> Declarations
borderHB1 = borderHB 1.px

export %inline
borderHB2 : Color -> Declarations
borderHB2 = borderHB 2.px

export %inline
borderHB3 : Color -> Declarations
borderHB3 = borderHB 3.px

export
round2 : Declaration
round2 = borderRadius 2.px

export
round4 : Declaration
round4 = borderRadius 4.px

export
round8 : Declaration
round8 = borderRadius 8.px

export
round16 : Declaration
round16 = borderRadius 16.px

export
round32 : Declaration
round32 = borderRadius 32.px

export
round : Declaration
round = borderRadius 50.perc

--------------------------------------------------------------------------------
-- Padding
--------------------------------------------------------------------------------

export
noPadding : Declaration
noPadding = padding (All 0.px)

--------------------------------------------------------------------------------
-- Margin
--------------------------------------------------------------------------------

export
noMargin : Declaration
noMargin = margin (All 0.px)

--------------------------------------------------------------------------------
-- Elem Dimensions
--------------------------------------------------------------------------------

export
exactWidth : Width -> Declarations
exactWidth w = [minWidth w, maxWidth w, width w]

export
exactHeight : Width -> Declarations
exactHeight h = [minHeight h, maxHeight h, height h]

--------------------------------------------------------------------------------
-- Flex Containers
--------------------------------------------------------------------------------

export
noflex : Declaration
noflex = flex "0 0 auto"

export
flex1 : Declaration
flex1 = flex "1"

export
flex2 : Declaration
flex2 = flex "2"

export
flex3 : Declaration
flex3 = flex "3"

export
flex4 : Declaration
flex4 = flex "4"

export
flexRow : Declarations
flexRow = [display Flex, flexDirection Row]

export
startRow : Declarations
startRow = alignItems Start :: flexRow

export
centerRow : Declarations
centerRow = alignItems Center :: flexRow

export
endRow : Declarations
endRow = alignItems Center :: flexRow

export
stretchRow : Declarations
stretchRow = alignItems Stretch :: flexRow

export
flexCol : Declarations
flexCol = [display Flex, flexDirection Column]

export
startCol : Declarations
startCol = alignItems Start :: flexCol

export
centerCol : Declarations
centerCol = alignItems Center :: flexCol

export
endCol : Declarations
endCol = alignItems Center :: flexCol

export
stretchCol : Declarations
stretchCol = alignItems Stretch :: flexCol

parameters {default 0.5.em gap : Length}

  export
  flexSepRow : Declarations
  flexSepRow = [display Flex, columnGap gap, flexDirection Row]

  export
  startSepRow : Declarations
  startSepRow = alignItems Start :: flexSepRow

  export
  centerSepRow : Declarations
  centerSepRow = alignItems Center :: flexSepRow

  export
  endSepRow : Declarations
  endSepRow = alignItems Center :: flexSepRow

  export
  stretchSepRow : Declarations
  stretchSepRow = alignItems Stretch :: flexSepRow

  export
  flexSepCol : Declarations
  flexSepCol = [display Flex, rowGap gap, flexDirection Column]

  export
  startSepCol : Declarations
  startSepCol = alignItems Start :: flexSepCol

  export
  centerSepCol : Declarations
  centerSepCol = alignItems Center :: flexSepCol

  export
  endSepCol : Declarations
  endSepCol = alignItems Center :: flexSepCol

  export
  stretchSepCol : Declarations
  stretchSepCol = alignItems Stretch :: flexSepCol
