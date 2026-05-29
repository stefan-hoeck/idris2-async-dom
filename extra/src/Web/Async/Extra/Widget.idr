module Web.Async.Extra.Widget

import Data.List
import HTTP.API.Decode
import Text.HTML.Extra
import public Web.Async.Extra.I18n
import public Web.Async

%default total

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

data LogEv = Clear | Lvl LogLevel

printErr : JSErr -> JS [] ()
printErr x = putStrLn "Error: \{dispErr x}"

export
logNode : HTTPLocal => LogLevel -> List String -> HTMLNode
logNode l msgs =
  li []
    [ label [class $ level l] [Text $ "[\{logLevel l}]"]
    , div [] $ intersperse (br []) (map Text msgs)
    ]

export
uilog : HTTPLocal => (ref : IORef LogLevel) => Logger JS
uilog =
  MkLogger $ \l,ml => Prelude.do
    x <- readref ref
    when (l >= x) $ handle [printErr] (prepend (elemRef AsyncLog) $ logNode l ml)

levels : List LogLevel
levels = [Trace,Debug,Info,Warn,Error,Fatal]

appLog : Sink LogEv => ExtraLocal => HTMLNode
appLog =
  section
    [ class asyncLog ]
    [ header []
        [ label [] [Text logTxt]
        , spacer
        , button [onClick Clear] [Text clearTxt]
        , selectFromList' levels (Just Info) show Lvl []
        ]
    , ul [ref AsyncLog] []
    ]

public export
record Logger where
  constructor L
  node   : HTMLNode
  stream : AsyncStream JS [] Void
  logger : Logger JS

onev : (ref : IORef LogLevel) => LogEv -> Async JS [] ()
onev Clear   = handle [printErr] $ children (elemRef AsyncLog) []
onev (Lvl x) = writeref ref x

export
logger : ExtraLocal => LogLevel -> Act Logger
logger l = Prelude.do
  ref  <- newref l
  E es <- event LogEv
  pure $ L appLog (foreach onev es) uilog

--------------------------------------------------------------------------------
-- Input Validation
--------------------------------------------------------------------------------

validIcon : DOMLocal => Ref Tag.Div -> EditRes t -> HTMLNode
validIcon r (Valid _)   = hiddenDiv r
validIcon r (Invalid s) = div [class iconError, Id r, title s] [iwarn]
validIcon r Missing     =
  div [class iconMissing, Id r, title $ editRes {t} Missing] [iwarn]

export
validated : DOMLocal => Editor t -> Editor t
validated ed =
  E $ \m => Prelude.do
    lbl     <- uniqueRef Tag.Div
    W ns vs <- ed.widget m
    pure $ W
      [div [class validatedInput] $ ns ++ [validIcon lbl $ Missing {t}]]
      (observe (replace lbl . validIcon lbl) vs)

--------------------------------------------------------------------------------
-- Select Editors
--------------------------------------------------------------------------------

parameters {auto loc : DOMLocal}
           {auto eq  : Eq t}

  export %inline
  seledit : Maybe Class -> (v -> t) -> (v -> String) -> List v -> Editor t
  seledit c f g vs = E $ Widget.sel f g vs $ class <$> toList c

  export %inline
  selEdit : (v -> t) -> (v -> String) -> List v -> Editor t
  selEdit = seledit Nothing

  export %inline
  selEditC : Class -> (v -> t) -> (v -> String) -> List v -> Editor t
  selEditC = seledit . Just

--------------------------------------------------------------------------------
-- Input Editors
--------------------------------------------------------------------------------

parameters {auto loc : DOMLocal}

  export
  input :
       (dec : String -> EditRes t)
    -> InputType
    -> (init : Maybe t -> String)
    -> Editor t
  input dec tpe init = validated $ txtEdit dec tpe init []

  ||| Specialized version of `input` for entering floating point numbers.
  export
  double : Editor Double
  double = input read Text (maybe "0.0" show)

  ||| Specialized version of `input` for natural numbers.
  export
  nat : Editor Nat
  nat = input read Text (maybe "0" show)

  ||| Specialized version of `input` for entering integers.
  export
  integer : Editor Integer
  integer = input read Text (maybe "0" show)

  ||| Specialized version of `input` for entering 8-bit unsigned integers.
  export
  bits8 : Editor Bits8
  bits8 = input read Text (maybe "0" show)

  ||| Specialized version of `input` for entering 16-bit unsigned integers.
  export
  bits16 : Editor Bits16
  bits16 = input read Text (maybe "0" show)

  ||| Specialized version of `input` for entering 32-bit unsigned integers.
  export
  bits32 : Editor Bits32
  bits32 = input read Text (maybe "0" show)

  ||| Specialized version of `input` for entering 64-bit unsigned integers.
  export
  bits64 : Editor Bits64
  bits64 = input read Text (maybe "0" show)

  ||| Specialized version of `input` for entering 8-bit signed integers.
  export
  int8 : Editor Int8
  int8 = input read Text (maybe "0" show)

  ||| Specialized version of `input` for entering 16-bit signed integers.
  export
  int16 : Editor Int16
  int16 = input read Text (maybe "0" show)

  ||| Specialized version of `input` for entering 32-bit signed integers.
  export
  int32 : Editor Int32
  int32 = input read Text (maybe "0" show)

  ||| Specialized version of `input` for entering 64-bit signed integers.
  export
  int64 : Editor Int64
  int64 = input read Text (maybe "0" show)
