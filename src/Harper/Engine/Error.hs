module Harper.Engine.Error
where

import Harper.Abs
import Harper.Abs.Pos
import Harper.Engine.Output
import Harper.Printer

runtimeErr :: HarperOutput a
runtimeErr = fail "runtime error."

invType :: (Print p, Position p) => String -> String -> Value Pos -> p -> HarperOutput a
invType act exp v ctx = do
    outputErr (("value `"++) . showsPrt v . ("` has invalid type '"++) . (act ++) . 
        ("'; expected '"++) . (exp ++) . ("'."++)) ctx
    runtimeErr

invTypes :: (Print p, Position p) => String -> String -> String -> p -> p -> p -> HarperOutput a
invTypes act1 act2 exp v1 v2 ctx = do
    outputErr (
        ("values `"++) . showsPrt v1 . ("` and `"++) . showsPrt v2 . ("` have invalid types '"++) . 
        (act1 ++) . ("', '"++) . (act2++) . ("'; expected '"++) . (exp++) . ("'."++)
        ) ctx
    runtimeErr
    
invEqTypes :: (Print p, Position p) => String -> String -> p -> p -> p -> HarperOutput a
invEqTypes act1 act2 v1 v2 ctx = do
    outputErr (
        ("values `"++) . showsPrt v1 . ("` and `"++) . showsPrt v2 . ("` have invalid types '"++) . 
        (act1 ++) . ("', '"++) . (act2++) . ("'; only values of the same primitive type can be equated."++)
        ) ctx
    runtimeErr

invCmpTypes :: (Print p, Position p) => String -> String -> p -> p -> p -> HarperOutput a
invCmpTypes act1 act2 v1 v2 ctx = do
    outputErr (
        ("values `"++) . showsPrt v1 . ("` and `"++) . showsPrt v2 . ("` have invalid types '"++) . 
        (act1 ++) . ("', '"++) . (act2++) . ("'; only values of the same primitive type can be compared."++)
        ) ctx
    runtimeErr

overApp :: Value Pos -> HarperOutput a
overApp v = do
    outputErr ("function applied to too many arguments."++) v
    runtimeErr

noReturn :: Statement Pos -> HarperOutput a
noReturn s = do
    outputErr ("control reached end of function without a return statement."++) s
    runtimeErr

divByZero :: (Print p, Position p) => Value Pos -> p -> HarperOutput a
divByZero v ctx = do
    outputErr (("value `"++) . showsPrt v . ("` evaluated to zero causing a division error."++)) ctx
    runtimeErr