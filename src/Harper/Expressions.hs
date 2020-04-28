module Harper.Expressions where

import           Harper.Abs
import           Harper.Interpreter.Core
import           Harper.TypeSystem.Core         ( Type(..) )
import           Harper.TypeSystem.GlobalTypes

litTrue :: Expression Meta
litTrue =
    let meta = (boolT, Nothing)
    in  LitExpr meta (BoolLit meta (BTrue meta))

litFalse :: Expression Meta
litFalse =
    let meta = (boolT, Nothing)
    in  LitExpr meta (BoolLit meta (BFalse meta))

litUnit :: Expression Meta
litUnit =
    let meta = (unitT, Nothing)
    in  LitExpr meta (UnitLit meta)

