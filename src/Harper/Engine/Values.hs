module Harper.Engine.Values
where

import Harper.Abs

litTrue :: Value Pos
litTrue = LitVal Nothing (BoolLit Nothing (BTrue Nothing))

litFalse :: Value Pos
litFalse = LitVal Nothing (BoolLit Nothing (BFalse Nothing))

unit :: Value Pos
unit = UnitVal Nothing -- TODO: Turn into a literal.