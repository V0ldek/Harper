module Harper.Engine.Error where
import           Control.Monad.Trans
import           Data.List

import           Harper.Abs
import           Harper.Abs.Pos
import           Harper.Engine.Core
import           Harper.Engine.Output
import           Harper.Printer

raise :: HarperOutput a -> Interpreter a
raise = lift . lift

runtimeErr :: HarperOutput a
runtimeErr = fail "runtime error."

invType
    :: (Print p, Position p)
    => String
    -> String
    -> Expression Pos
    -> p
    -> HarperOutput a
invType act exp e ctx = do
    outputErr
        ( ("expression `" ++)
        . showsPrt e
        . ("` has invalid type '" ++)
        . (act ++)
        . ("'; expected '" ++)
        . (exp ++)
        . ("'." ++)
        )
        ctx
    runtimeErr

invTypes
    :: (Print p, Position p)
    => String
    -> String
    -> String
    -> p
    -> p
    -> p
    -> HarperOutput a
invTypes act1 act2 exp e1 e2 ctx = do
    outputErr
        ( ("expressions `" ++)
        . showsPrt e1
        . ("` and `" ++)
        . showsPrt e2
        . ("` have invalid types '" ++)
        . (act1 ++)
        . ("', '" ++)
        . (act2 ++)
        . ("'; expected '" ++)
        . (exp ++)
        . ("'." ++)
        )
        ctx
    runtimeErr

invEqTypes
    :: (Print p, Position p)
    => String
    -> String
    -> p
    -> p
    -> p
    -> HarperOutput a
invEqTypes act1 act2 e1 e2 ctx = do
    outputErr
        ( ("expressions `" ++)
        . showsPrt e1
        . ("` and `" ++)
        . showsPrt e2
        . ("` have invalid types '" ++)
        . (act1 ++)
        . ("', '" ++)
        . (act2 ++)
        . ("'; only values of the same primitive type can be equated." ++)
        )
        ctx
    runtimeErr

invCmpTypes
    :: (Print p, Position p)
    => String
    -> String
    -> p
    -> p
    -> p
    -> HarperOutput a
invCmpTypes act1 act2 e1 e2 ctx = do
    outputErr
        ( ("expressions `" ++)
        . showsPrt e1
        . ("` and `" ++)
        . showsPrt e2
        . ("` have invalid types '" ++)
        . (act1 ++)
        . ("', '" ++)
        . (act2 ++)
        . ("'; only values of the same primitive type can be compared." ++)
        )
        ctx
    runtimeErr

overApp :: Expression Pos -> HarperOutput a
overApp e = do
    outputErr ("function applied to too many arguments." ++) e
    runtimeErr

noReturn :: Statement Pos -> HarperOutput a
noReturn s = do
    outputErr
        ("control reached end of function without a return statement." ++)
        s
    runtimeErr

divByZero :: (Print p, Position p) => Expression Pos -> p -> HarperOutput a
divByZero e ctx = do
    outputErr
        ( ("expression `" ++)
        . showsPrt e
        . ("` evaluated to zero causing a division error." ++)
        )
        ctx
    runtimeErr

assToValue :: Ident -> Statement Pos -> HarperOutput a
assToValue i ctx = do
    outputErr
        ( ("cannot assign to an immutable variable `" ++)
        . showsPrt i
        . ("`." ++)
        )
        ctx
    runtimeErr

invAss :: String -> Ident -> Statement Pos -> HarperOutput a
invAss t i ctx = do
    outputErr
        ( ("cannot assign to `" ++)
        . showsPrt i
        . ("`, which is a value of type '" ++)
        . (t ++)
        . ("'." ++)
        )
        ctx
    runtimeErr

undeclaredIdent :: (Print p, Position p) => Ident -> p -> HarperOutput a
undeclaredIdent i ctx = do
    outputErr (("undeclared identifier `" ++) . showsPrt i . ("`." ++)) ctx
    runtimeErr

undeclaredUIdent :: (Print p, Position p) => UIdent -> p -> HarperOutput a
undeclaredUIdent i ctx = do
    outputErr (("undeclared type identifier `" ++) . showsPrt i . ("`." ++)) ctx
    runtimeErr

unassVar :: (Print p, Position p) => Ident -> p -> HarperOutput a
unassVar i ctx = do
    outputErr
        (("variable `" ++) . showsPrt i . ("` used before it was assigned." ++))
        ctx
    runtimeErr

nonExhPatMatch :: (Print p, Position p) => p -> HarperOutput a
nonExhPatMatch ctx = do
    outputErr ("non exhaustive pattern match." ++) ctx
    runtimeErr

invFldAcc :: (Print p, Position p) => Type -> Ident -> p -> HarperOutput a
invFldAcc t i ctx = do
    outputErr
        (("type `" ++) . shows t . ("` has no field `" ++) . shows i . ("`." ++)
        )
        ctx
    runtimeErr

unassFlds :: (Print p, Position p) => Type -> [Ident] -> p -> HarperOutput a
unassFlds t is ctx = do
    outputErr
        (("all fields must be assigned during value construction. Unassigned fields for `" ++
         )
        . shows t
        . ("` are: `" ++)
        . flds
        . ("`." ++)
        )
        ctx
    runtimeErr
    where flds = foldr (.) id $ intersperse ("`, `" ++) $ map showsPrt is

excessFlds :: (Print p, Position p) => Type -> [Ident] -> p -> HarperOutput a
excessFlds t is ctx = do
    outputErr
        (("unrecognized field identifiers during value construction. Type `" ++)
        . shows t
        . ("` has no fields: `" ++)
        . flds
        . ("`." ++)
        )
        ctx
    runtimeErr
    where flds = foldr (.) id $ intersperse ("`, `" ++) $ map showsPrt is
