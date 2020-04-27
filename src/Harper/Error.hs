module Harper.Error where
import           Control.Monad.Trans
import           Data.List

import           Harper.Abs
import           Harper.Abs.Pos
import           Harper.Interpreter.Core
import           Harper.TypeSystem.Core         ( Type
                                                , TypeCtor
                                                )
import           Harper.Output
import           Harper.Printer

runtimeErr :: HarperOutput a
runtimeErr = fail "runtime error."

typeErr :: HarperOutput a
typeErr = fail "type error."

invType
    :: (Position p1, Print p2, Position p2)
    => Type
    -> Type
    -> Expression p1
    -> p2
    -> HarperOutput a
invType t1 t2 e ctx = do
    outputErr
        ( ("expression `" ++)
        . showsPrt e
        . ("` has invalid type '" ++)
        . shows t1
        . ("'; expected '" ++)
        . shows t2
        . ("'." ++)
        )
        ctx
    typeErr

invTypes
    :: (Print p, Position p)
    => Type
    -> Type
    -> Type
    -> p
    -> p
    -> p
    -> HarperOutput a
invTypes t1 t2 t e1 e2 ctx = do
    outputErr
        ( ("expressions `" ++)
        . showsPrt e1
        . ("` and `" ++)
        . showsPrt e2
        . ("` have invalid types '" ++)
        . shows t1
        . ("', '" ++)
        . shows t2
        . ("'; expected '" ++)
        . shows t
        . ("'." ++)
        )
        ctx
    typeErr

invEqTypes
    :: (Print p, Position p) => Type -> Type -> p -> p -> p -> HarperOutput a
invEqTypes t1 t2 e1 e2 ctx = do
    outputErr
        ( ("expressions `" ++)
        . showsPrt e1
        . ("` and `" ++)
        . showsPrt e2
        . ("` have invalid types '" ++)
        . shows t1
        . ("', '" ++)
        . shows t2
        . ("'; only values of the same, non function type can be equated." ++)
        )
        ctx
    typeErr

invCmpTypes
    :: (Print p, Position p) => Type -> Type -> p -> p -> p -> HarperOutput a
invCmpTypes t1 t2 e1 e2 ctx = do
    outputErr
        ( ("expressions `" ++)
        . showsPrt e1
        . ("` and `" ++)
        . showsPrt e2
        . ("` have invalid types '" ++)
        . shows t1
        . ("', '" ++)
        . shows t2
        . ("'; only values of the same primitive type can be compared." ++)
        )
        ctx
    typeErr

invPredType
    :: (Position p1, Print p2, Position p2)
    => Type
    -> Expression p1
    -> p2
    -> HarperOutput a
invPredType t e ctx = do
    outputErr
        ( ("expression `" ++)
        . showsPrt e
        . ("` of type '" ++)
        . shows t
        . ("' cannot be used as a predicate in a conditional." ++)
        )
        ctx
    typeErr

invApp :: (Position p) => Type -> Expression p -> HarperOutput a
invApp t1 ctx = do
    outputErr
        (("type '" ++)
        . shows t1
        . ("' is not a function type and a value of this type cannot have arguments applied to it. Possibly caused by appling too many arguments to a function." ++
          )
        )
        ctx
    typeErr

noRet :: (Position p) => Statement p -> HarperOutput a
noRet s = do
    outputErr
        ("control may reach the end of a non Unit valued function without a return statement." ++)
        s
    typeErr

divByZero
    :: (Position p1, Print p2, Position p2)
    => Expression p1
    -> p2
    -> HarperOutput a
divByZero e ctx = do
    outputErr
        ( ("expression `" ++)
        . showsPrt e
        . ("` evaluated to zero causing a division error." ++)
        )
        ctx
    runtimeErr

assToValue :: (Position p) => Ident -> Statement p -> HarperOutput a
assToValue i ctx = do
    outputErr
        ( ("cannot assign to an immutable variable `" ++)
        . showsPrt i
        . ("`." ++)
        )
        ctx
    runtimeErr

undeclaredIdent :: (Print p, Position p) => Ident -> p -> HarperOutput a
undeclaredIdent i ctx = do
    outputErr (("undeclared identifier `" ++) . showsPrt i . ("`." ++)) ctx
    typeErr

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

invFldAcc :: (Print p, Position p) => TypeCtor -> Ident -> p -> HarperOutput a
invFldAcc t i ctx = do
    outputErr
        (("type '" ++) . shows t . ("' has no field `" ++) . shows i . ("`." ++)
        )
        ctx
    typeErr

unassFlds :: (Print p, Position p) => TypeCtor -> [Ident] -> p -> HarperOutput a
unassFlds t is ctx = do
    outputErr
        (("all fields must be assigned during value construction. Unassigned fields for '" ++
         )
        . shows t
        . ("' are: `" ++)
        . flds
        . ("`." ++)
        )
        ctx
    typeErr
    where flds = foldl' (.) id $ intersperse ("`, `" ++) $ map showsPrt is

excessFlds
    :: (Print p, Position p) => TypeCtor -> [Ident] -> p -> HarperOutput a
excessFlds t is ctx = do
    outputErr
        (("unrecognized field identifiers during value construction. Type '" ++)
        . shows t
        . ("' has no fields: `" ++)
        . flds
        . ("`." ++)
        )
        ctx
    typeErr
    where flds = foldl' (.) id $ intersperse ("`, `" ++) $ map showsPrt is

conflTypeNames
    :: (Print p1, Position p1, Print p2, Position p2)
    => Ident
    -> p1
    -> p2
    -> HarperOutput a
conflTypeNames i ctx1 ctx2 = do
    outputConfl (("conflicting type name `" ++) . showsPrt i . ("`." ++))
                ctx1
                ctx2
    typeErr

conflCtorNames
    :: (Print p1, Position p1, Print p2, Position p2)
    => UIdent
    -> p1
    -> p2
    -> HarperOutput a
conflCtorNames i ctx1 ctx2 = do
    outputConfl (("conflicting ctor name `" ++) . showsPrt i . ("`." ++))
                ctx1
                ctx2
    typeErr

conflTypeParam :: (Print p, Position p) => Ident -> p -> HarperOutput a
conflTypeParam i ctx = do
    outputErr
        (("conflicting type parameter name `" ++) . showsPrt i . ("`." ++))
        ctx
    typeErr

typeInvArity
    :: (Print p, Position p) => Type -> Int -> Int -> p -> HarperOutput a
typeInvArity t tArity nArgs ctx = do
    outputErr
        ( ("the type '" ++)
        . shows t
        . ("' is applied to " ++)
        . shows nArgs
        . (" type arguments, but it has arity " ++)
        . shows tArity
        . ("." ++)
        )
        ctx
    typeErr

unboundTypeVar :: (Print p, Position p) => Ident -> p -> HarperOutput a
unboundTypeVar i ctx = do
    outputErr
        ( ("unbound type variable identifier `" ++)
        . showsPrt i
        . ("` outside of a type signature declaration." ++)
        )
        ctx
    typeErr

funWOutType :: (Print p, Position p) => Ident -> p -> HarperOutput a
funWOutType i ctx = do
    outputErr
        (("function `" ++)
        . showsPrt i
        . ("` has no associated type signature and its type cannot be inferred." ++
          )
        )
        ctx
    typeErr

locWOutType :: (Print p, Position p) => Ident -> p -> HarperOutput a
locWOutType i ctx = do
    outputErr
        (("local `" ++)
        . showsPrt i
        . ("` has no associated type signature and its type cannot be inferred." ++
          )
        )
        ctx
    typeErr

nonFunDeclWithParams
    :: (Print p, Position p) => Ident -> Type -> p -> HarperOutput a
nonFunDeclWithParams i t ctx = do
    outputErr
        ( ("object `" ++)
        . showsPrt i
        . ("` has a type '" ++)
        . shows t
        . ("', which is not a function, but its declaration has formal parameters." ++
          )
        )
        ctx
    typeErr

funInvType
    :: (Print p, Position p) => Ident -> Type -> Type -> p -> HarperOutput a
funInvType i exp act ctx = do
    outputErr
        ( ("cannot match function's `" ++)
        . showsPrt i
        . ("` declared type '" ++)
        . shows exp
        . ("' with its actual type '" ++)
        . shows act
        . ("'." ++)
        )
        ctx
    typeErr

tooManyParams
    :: (Print p, Position p)
    => Ident
    -> Type
    -> Int
    -> Int
    -> p
    -> HarperOutput a
tooManyParams i t n1 n2 ctx = do
    outputErr
        ( ("function `" ++)
        . showsPrt i
        . ("` has a type '" ++)
        . shows t
        . ("', which is of arity " ++)
        . shows n1
        . (", but it declares " ++)
        . shows n2
        . (" formal parameters." ++)
        )
        ctx
    typeErr

conflMatchClauseTypes :: (Print p, Position p) => [Type] -> p -> HarperOutput a
conflMatchClauseTypes ts ctx = do
    outputErr
        (("cannot unify conflicting types of match clauses: " ++) . printTypes)
        ctx
    typeErr
    where printTypes = foldl' (.) id $ intersperse ("', '" ++) $ map shows ts

conflRetTypes :: (Print p, Position p) => [Type] -> p -> HarperOutput a
conflRetTypes ts ctx = do
    outputErr
        (("cannot unify conflicting return types of a function: " ++) . printTypes)
        ctx
    typeErr
    where printTypes = foldl' (.) id $ intersperse ("', '" ++) $ map shows ts

conflFldSubsts :: (Print p, Position p) => Type -> TypeCtor -> [Type] -> p -> HarperOutput a
conflFldSubsts t ctor ts ctx = do
    outputErr
        (("cannot unify conflicting types: " ++) . printTypes
        . (" resulting from field patterns applied to the type constructor `"++)
        . shows ctor
        . ("` of type '"++)
        . shows t
        . ("'."++))
        ctx
    typeErr
    where printTypes = foldl' (.) id $ intersperse ("`, `" ++) $ map shows ts

patInvType :: (Print p, Position p) => Type -> Type -> p -> HarperOutput a
patInvType tAct tExp ctx = do
    outputErr
        ( ("the pattern of type '" ++)
        . shows tAct
        . ("' cannot be used to match an expression of type '" ++)
        . shows tExp
        . ("'." ++)
        )
        ctx
    typeErr

undeclaredCtor :: (Print p, Position p) => UIdent -> p -> HarperOutput a
undeclaredCtor i ctx = do
    outputErr (("undeclared type constructor `" ++) . showsPrt i . ("`." ++)) ctx
    typeErr

undeclaredType :: (Print p, Position p) => UIdent -> p -> HarperOutput a
undeclaredType i ctx = do
    outputErr (("undeclared type identifier `" ++) . showsPrt i . ("`." ++)) ctx
    typeErr

invCtor :: (Print p, Position p) => Type -> UIdent -> p -> HarperOutput a
invCtor t i ctx = do
    outputErr
        ( ("the type '" ++)
        . shows t
        . ("' does not have a constructor `" ++)
        . showsPrt i
        . ("`." ++)
        )
        ctx
    typeErr
