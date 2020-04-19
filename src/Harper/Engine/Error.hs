module Harper.Engine.Error where

import           Harper.Abs
import           Harper.Abs.Pos
import           Harper.Engine.Output
import           Harper.Printer

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
