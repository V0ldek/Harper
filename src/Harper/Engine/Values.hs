module Harper.Engine.Values
where

import Harper.Abs

litTrue :: Value
litTrue = LitVal (BoolLit BTrue)

litFalse :: Value
litFalse = LitVal (BoolLit BFalse)

unit :: Value
unit = UnitVal -- TODO: Turn into a literal.