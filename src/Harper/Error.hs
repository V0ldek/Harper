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

invMainType :: HarperOutput a
invMainType =
    fail "function `main` has invalid type. Only parameterless mains are valid."

undeclaredMain :: HarperOutput a
undeclaredMain = fail "function `main` was not declared."

showsMany :: (Show a) => [a] -> ShowS
showsMany xs = foldl' (.) id $ intersperse ("`, `" ++) $ map shows xs

showsPrtMany :: (Print p) => [p] -> ShowS
showsPrtMany xs = foldl' (.) id $ intersperse ("`, `" ++) $ map showsPrt xs

assToValue :: (Print p, Position p) => Ident -> p -> HarperOutput a
breakOutsideOfLoop :: (Print p, Position p) => p -> HarperOutput a
conflCtorNames :: (Print p, Position p) => [UIdent] -> [p] -> HarperOutput a
conflFldNames
    :: (Print p, Position p) => UIdent -> [Ident] -> [p] -> HarperOutput a
conflFldSubsts
    :: (Print p, Position p)
    => Type
    -> TypeCtor
    -> [Type]
    -> p
    -> HarperOutput a
conflMatchClauseTypes :: (Print p, Position p) => [Type] -> p -> HarperOutput a
conflMembNames
    :: (Print p, Position p) => UIdent -> [Ident] -> [p] -> HarperOutput a
conflMembTypes
    :: (Print p, Position p) => Type -> Ident -> [Type] -> [p] -> HarperOutput a
conflPatDecls :: (Print p, Position p) => [Ident] -> p -> HarperOutput a
conflRetTypes :: (Print p, Position p) => [Type] -> p -> HarperOutput a
conflTypeNames :: (Print p, Position p) => [Ident] -> [p] -> HarperOutput a
conflTypeParams :: (Print p, Position p) => [Ident] -> p -> HarperOutput a
conflYieldTypes :: (Print p, Position p) => [Type] -> p -> HarperOutput a
continueOutsideOfLoop :: (Print p, Position p) => p -> HarperOutput a
dataAccessInValueType :: (Print p, Position p) => Type -> p -> HarperOutput a
dataAccessOutsideOfMemb :: (Print p, Position p) => p -> HarperOutput a
divByZero
    :: (Position p1, Print p2, Position p2)
    => Expression p1
    -> p2
    -> HarperOutput a
excessFlds
    :: (Print p, Position p) => TypeCtor -> [Ident] -> p -> HarperOutput a
funInvType
    :: (Print p, Position p) => Ident -> Type -> Type -> p -> HarperOutput a
funWOutType :: (Print p, Position p) => Ident -> p -> HarperOutput a
invApp :: (Position p) => Type -> Expression p -> HarperOutput a
invCmpTypes
    :: (Print p, Position p) => Type -> Type -> p -> p -> p -> HarperOutput a
invCtorType :: (Print p, Position p) => Type -> Type -> p -> HarperOutput a
invEqTypes
    :: (Print p, Position p) => Type -> Type -> p -> p -> p -> HarperOutput a
invFldAcc :: (Print p, Position p) => TypeCtor -> Ident -> p -> HarperOutput a
invMembAccess :: (Print p, Position p) => Type -> Ident -> p -> HarperOutput a
invPredType
    :: (Position p1, Print p2, Position p2)
    => Type
    -> Expression p1
    -> p2
    -> HarperOutput a
invType
    :: (Position p1, Print p2, Position p2)
    => Type
    -> Type
    -> Expression p1
    -> p2
    -> HarperOutput a
invTypes
    :: (Print p, Position p)
    => Type
    -> Type
    -> Type
    -> p
    -> p
    -> p
    -> HarperOutput a
iterCurrNoElem :: HarperOutput a
localWOutType :: (Print p, Position p) => Ident -> p -> HarperOutput a
missingDeclaration :: (Print p, Position p) => Ident -> p -> HarperOutput a
mixingYieldAndReturn :: (Print p, Position p) => p -> HarperOutput a
noRet :: (Position p) => Statement p -> HarperOutput a
nonExhPatMatch :: (Print p, Position p) => p -> HarperOutput a
nonFunDeclWithParams
    :: (Print p, Position p) => Ident -> Type -> p -> HarperOutput a
notDeclOnVar
    :: (Print p, Position p) => UIdent -> UIdent -> Ident -> p -> HarperOutput a
notIterable
    :: (Position p1, Print p2, Position p2)
    => Type
    -> Type
    -> Expression p1
    -> p2
    -> HarperOutput a
patInvType :: (Print p, Position p) => Type -> Type -> p -> HarperOutput a
refTypeNoCtor :: (Print p, Position p) => Type -> p -> HarperOutput a
sideeffectNotUnitLit
    :: (Print p1, Position p1, Print p2) => p2 -> p1 -> HarperOutput a
thisOutsideOfMember :: (Print p, Position p) => p -> HarperOutput a
tooManyParams
    :: (Print p, Position p)
    => Ident
    -> Type
    -> Int
    -> Int
    -> p
    -> HarperOutput a
typeInvArity
    :: (Print p, Position p) => Type -> Int -> Int -> p -> HarperOutput a
unassFlds :: (Print p, Position p) => TypeCtor -> [Ident] -> p -> HarperOutput a
unassVar :: (Print p, Position p) => Ident -> p -> HarperOutput a
unboundTypeVar :: (Print p, Position p) => Ident -> p -> HarperOutput a
undeclaredCtor :: (Print p, Position p) => UIdent -> p -> HarperOutput a
undeclaredIdent :: (Print p, Position p) => Ident -> p -> HarperOutput a
undeclaredType :: (Print p, Position p) => UIdent -> p -> HarperOutput a
varNotDefAss :: (Print p, Position p) => Ident -> p -> HarperOutput a

assToValue i ctx = do
    outputErr
        (("cannot assign to an immutable value `" ++) . showsPrt i . ("`." ++))
        ctx
    typeErr

breakOutsideOfLoop ctx = do
    outputErr
        ("a `break` statement cannot be used outside of a loop body." ++)
        ctx
    typeErr

conflCtorNames is ctxs = do
    outputConflDecls
        (("conflicting type variant names `" ++) . showsPrtMany is . ("`." ++))
        ctxs
    typeErr

conflFldNames tName is ctxs = do
    outputConflDecls
        ( ("conflicting field names `" ++)
        . showsPrtMany is
        . ("` in the declaration of `" ++)
        . showsPrt tName
        . ("`." ++)
        )
        ctxs
    typeErr

conflFldSubsts t ctor ts ctx = do
    outputErr
        ( ("cannot unify conflicting types: `" ++)
        . showsMany ts
        . ("` resulting from field patterns applied to the type constructor `" ++
          )
        . shows ctor
        . ("` of type `" ++)
        . shows t
        . ("`." ++)
        )
        ctx
    typeErr

conflMatchClauseTypes ts ctx = do
    outputErr
        ( ("cannot unify conflicting types of match clauses: `" ++)
        . showsMany ts
        . ("`." ++)
        )
        ctx
    typeErr

conflMembNames ctor is ctxs = do
    outputConflDecls
        ( ("conflicting member declarations of `" ++)
        . showsPrtMany is
        . ("` in the declaration of `" ++)
        . showsPrt ctor
        . ("`." ++)
        )
        ctxs
    typeErr

conflMembTypes t i ts ctxs = do
    outputConflDecls
        ( ("cannot unify conflicting types: `" ++)
        . showsMany ts
        . ("` resulting from declarations of the member `" ++)
        . showsPrt i
        . ("` of the type `" ++)
        . shows t
        . ("`." ++)
        )
        ctxs
    typeErr

conflPatDecls is ctx = do
    outputErr
        ( ("conflicting variable identifiers `" ++)
        . showsPrtMany is
        . ("` in pattern." ++)
        )
        ctx
    typeErr

conflRetTypes ts ctx = do
    outputErr
        ( ("cannot unify conflicting return types of a function: `" ++)
        . showsMany ts
        . ("`." ++)
        )
        ctx
    typeErr

conflTypeNames is ctxs = do
    outputConflDecls
        (("conflicting type names `" ++) . showsPrtMany is . ("`." ++))
        ctxs
    typeErr

conflTypeParams is ctx = do
    outputErr
        (("conflicting type parameter names `" ++) . showsPrtMany is . ("`." ++)
        )
        ctx
    typeErr

conflYieldTypes ts ctx = do
    outputErr
        ( ("cannot unify conflicting yield types of a function: `" ++)
        . showsMany ts
        . ("`." ++)
        )
        ctx
    typeErr

continueOutsideOfLoop ctx = do
    outputErr
        ("a `continue` statement cannot be used outside of a loop body." ++)
        ctx
    typeErr

dataAccessInValueType t ctx = do
    outputErr
        (("`this.data` cannot be accessed within a member of `" ++)
        . shows t
        . ("`, which is a value type. Writable data access is only possible within members of ref types." ++
          )
        )
        ctx
    typeErr

dataAccessOutsideOfMemb ctx = do
    outputErr
        ("`this.data` can only be accessed within a member of a ref type." ++)
        ctx
    typeErr

divByZero e ctx = do
    outputErr
        ( ("expression `" ++)
        . showsPrt e
        . ("` evaluated to zero causing a division error." ++)
        )
        ctx
    runtimeErr

excessFlds t is ctx = do
    outputErr
        (("unrecognized field identifiers during value construction. Type `" ++)
        . shows t
        . ("` has no fields: `" ++)
        . showsPrtMany is
        . ("`." ++)
        )
        ctx
    typeErr

funInvType i exp act ctx = do
    outputErr
        ( ("cannot match function's `" ++)
        . showsPrt i
        . ("` declared type `" ++)
        . shows exp
        . ("` with its actual type `" ++)
        . shows act
        . ("`." ++)
        )
        ctx
    typeErr

funWOutType i ctx = do
    outputErr
        (("function `" ++)
        . showsPrt i
        . ("` has no associated type signature and its type cannot be inferred." ++
          )
        )
        ctx
    typeErr

invApp t1 ctx = do
    outputErr
        (("type `" ++)
        . shows t1
        . ("` is not a function type and a value of this type cannot have arguments applied to it. Possibly caused by appling too many arguments to a function." ++
          )
        )
        ctx
    typeErr

invCmpTypes t1 t2 e1 e2 ctx = do
    outputErr
        ( ("expressions `" ++)
        . showsPrt e1
        . ("` and `" ++)
        . showsPrt e2
        . ("` have invalid types `" ++)
        . shows t1
        . ("`, `" ++)
        . shows t2
        . ("`; only values of the same primitive type can be compared." ++)
        )
        ctx
    typeErr

invCtorType t ctor ctx = do
    outputErr
        ( ("the constructor of the type `" ++)
        . shows t
        . ("` has an invalid type `" ++)
        . shows ctor
        . ("`. The constructor must be an impure function returning an instance of `" ++
          )
        . shows t
        . ("`." ++)
        )
        ctx
    typeErr

invEqTypes t1 t2 e1 e2 ctx = do
    outputErr
        ( ("expressions `" ++)
        . showsPrt e1
        . ("` and `" ++)
        . showsPrt e2
        . ("` have invalid types `" ++)
        . shows t1
        . ("`, `" ++)
        . shows t2
        . ("`; only values of the same, non function type can be equated." ++)
        )
        ctx
    typeErr

invFldAcc t i ctx = do
    outputErr
        (("type `" ++) . shows t . ("` has no field `" ++) . shows i . ("`." ++)
        )
        ctx
    typeErr

invMembAccess t i ctx = do
    outputErr
        ( ("type `" ++)
        . shows t
        . ("` has no member `" ++)
        . showsPrt i
        . ("`." ++)
        )
        ctx
    typeErr

invPredType t e ctx = do
    outputErr
        ( ("expression `" ++)
        . showsPrt e
        . ("` of type `" ++)
        . shows t
        . ("` cannot be used as a predicate in a conditional." ++)
        )
        ctx
    typeErr

invType act exp e ctx = do
    outputErr
        ( ("expression `" ++)
        . showsPrt e
        . ("` has invalid type `" ++)
        . shows act
        . ("`; cannot match with `" ++)
        . shows exp
        . ("`." ++)
        )
        ctx
    typeErr

invTypes act1 act2 exp e1 e2 ctx = do
    outputErr
        ( ("expressions `" ++)
        . showsPrt e1
        . ("` and `" ++)
        . showsPrt e2
        . ("` have invalid types `" ++)
        . shows act1
        . ("`, `" ++)
        . shows act2
        . ("`; cannot match with `" ++)
        . shows exp
        . ("`." ++)
        )
        ctx
    typeErr

iterCurrNoElem = do
    outputErrInternal
        ("iterator's `current` function called before `next` or the sequence had no elements." ++
        )
    runtimeErr

localWOutType i ctx = do
    outputErr
        (("local `" ++)
        . showsPrt i
        . ("` has no associated type signature and its type cannot be inferred." ++
          )
        )
        ctx
    typeErr

missingDeclaration i ctx = do
    outputErr (("type hint for function `" ++) . showsPrt i . ("` has no associated definition." ++)) ctx
    typeErr

mixingYieldAndReturn ctx = do
    outputErr
        ("a `return` statement cannot be used in an iterator function." ++)
        ctx
    typeErr

noRet s = do
    outputErr
        ("control may reach the end of a non Unit valued function without a return statement." ++
        )
        s
    typeErr

nonExhPatMatch ctx = do
    outputErr ("non exhaustive pattern match." ++) ctx
    runtimeErr

nonFunDeclWithParams i t ctx = do
    outputErr
        ( ("object `" ++)
        . showsPrt i
        . ("` has a type `" ++)
        . shows t
        . ("`, which is not a function, but its declaration has formal parameters." ++
          )
        )
        ctx
    typeErr

notDeclOnVar t ctor memb ctx = do
    outputErr
        ( ("the variant `" ++)
        . showsPrt ctor
        . ("` of `" ++)
        . showsPrt t
        . ("` does not implement the function `" ++)
        . showsPrt memb
        . ("`." ++)
        )
        ctx
    runtimeErr

notIterable t1 t2 e ctx = do
    outputErr
        ( ("expression `" ++)
        . showsPrt e
        . ("` of type `" ++)
        . shows t1
        . ("` does not implement `Iterable " ++)
        . showsPrec 3 t2
        . ("` or `RefIterable " ++)
        . showsPrec 3 t2
        . (" impure` or `RefIterable " ++)
        . showsPrec 3 t2
        . (" sideeffect` and cannot be used in a `for` `in` statement." ++)
        )
        ctx
    typeErr

patInvType tAct tExp ctx = do
    outputErr
        ( ("the pattern of type `" ++)
        . shows tAct
        . ("` cannot be used to match an expression of type `" ++)
        . shows tExp
        . ("`." ++)
        )
        ctx
    typeErr

refTypeNoCtor t ctx = do
    outputErr
        (("the type `" ++)
        . shows t
        . ("` does not declare a constructor. A suitable `ctor` function is required." ++
          )
        )
        ctx
    typeErr

sideeffectNotUnitLit e ctx = do
    outputErr
        (("the expression `" ++)
        . showsPrt e
        . ("` of type `Unit` cannot be used to execute a `sideeffect` function. Did you mean to use a unit literal `()`?" ++
          )
        )
        ctx
    typeErr

thisOutsideOfMember ctx = do
    outputErr
        ("cannot use the `this` identifier outside of a member function." ++)
        ctx
    typeErr

tooManyParams i t n1 n2 ctx = do
    outputErr
        ( ("function `" ++)
        . showsPrt i
        . ("` has a type `" ++)
        . shows t
        . ("`, which is of arity " ++)
        . shows n1
        . (", but it declares " ++)
        . shows n2
        . (" formal parameters." ++)
        )
        ctx
    typeErr

typeInvArity t tArity nArgs ctx = do
    outputErr
        ( ("the type `" ++)
        . shows t
        . ("` is applied to " ++)
        . shows nArgs
        . (" type arguments, but it has arity " ++)
        . shows tArity
        . ("." ++)
        )
        ctx
    typeErr

unassFlds t is ctx = do
    outputErr
        (("all fields must be assigned during value construction. Unassigned fields for `" ++
         )
        . shows t
        . ("` are: `" ++)
        . showsPrtMany is
        . ("`." ++)
        )
        ctx
    typeErr

unassVar i ctx = do
    outputErr
        (("variable `" ++) . showsPrt i . ("` used before it was assigned." ++))
        ctx
    runtimeErr

unboundTypeVar i ctx = do
    outputErr
        ( ("unbound type variable identifier `" ++)
        . showsPrt i
        . ("` outside of a type signature declaration." ++)
        )
        ctx
    typeErr

undeclaredCtor i ctx = do
    outputErr
        (("undeclared type constructor `" ++) . showsPrt i . ("`." ++))
        ctx
    typeErr

undeclaredIdent i ctx = do
    outputErr (("undeclared identifier `" ++) . showsPrt i . ("`." ++)) ctx
    typeErr

undeclaredType i ctx = do
    outputErr (("undeclared type identifier `" ++) . showsPrt i . ("`." ++)) ctx
    typeErr

varNotDefAss i ctx = do
    outputErr
        ( ("variable `" ++)
        . showsPrt i
        . ("` is not definitely assigned at point of use." ++)
        )
        ctx
    typeErr