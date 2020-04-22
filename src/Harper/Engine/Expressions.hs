module Harper.Engine.Expressions where

import           Harper.Abs

litTrue :: Expression Pos
litTrue = LitExpr Nothing (BoolLit Nothing (BTrue Nothing))

litFalse :: Expression Pos
litFalse = LitExpr Nothing (BoolLit Nothing (BFalse Nothing))

litUnit :: Expression Pos
litUnit = LitExpr Nothing (UnitLit Nothing)

