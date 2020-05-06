module Harper.TypeSystem.GlobalTypes
    ( loadGlobalTypes
    , integerT
    , boolT
    , charT
    , stringT
    , unitT
    , iterateI
    , iterCurrentI
    , iterNextI
    , iteratorT
    , refIteratorT
    , iteratorI
    , refIteratorI
    , iterableI
    , refIterableI
    )
where
import           Control.Monad.State
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           Harper.Abs
import           Harper.TypeSystem.Alloc
import           Harper.TypeSystem.Core
import           Harper.TypeSystem.Typing

loadGlobalTypes :: TypeChecker ()
loadGlobalTypes = do
    loadTypes $ Map.fromList
        [ (UIdent "Integer", integerT)
        , (UIdent "Bool"   , boolT)
        , (UIdent "Char"   , charT)
        , (UIdent "String" , stringT)
        , (UIdent "Unit"   , unitT)
        ]
    var1 <- newvar
    var2 <- newvar
    var3 <- newvar
    _    <- iteratorT (TypeVar var1)
    _    <- refIteratorT (TypeVar var2) (TypeVar var3)
    _    <- iterableT
    return ()

integerT :: Type
integerT = PType (UIdent "Integer")

boolT :: Type
boolT = PType (UIdent "Bool")

charT :: Type
charT = PType (UIdent "Char")

stringT :: Type
stringT = PType (UIdent "String")

unitT :: Type
unitT = PType (UIdent "Unit")

createType
    :: (UIdent -> [Ident] -> [Type] -> Map.Map Ident Ptr -> IfaceStore -> Type)
    -> UIdent
    -> Integer
    -> [Ident]
    -> (Type -> [Type] -> TypeChecker [ObjData])
    -> ([Type] -> IfaceStore)
    -> TypeChecker Type
createType typeCtor tName arity membIs membOs ifaces = do
    ls <- newlocs (length membIs)
    let params = tParams arity
        args   = map TypeVar params
        isls   = zip membIs ls
        t'     = typeCtor tName params args (Map.fromList isls) (ifaces args)
    membs <- membOs t' args
    let lsos = zip ls membs
    modifyObjData (Map.union $ Map.fromList lsos)
    loadTypes $ Map.fromList [(tName, t')]
    return t'

createValType
    :: [UIdent]
    -> UIdent
    -> Integer
    -> [Ident]
    -> (Type -> [Type] -> TypeChecker [ObjData])
    -> ([Type] -> IfaceStore)
    -> TypeChecker Type
createValType ctorNames = createType
    (\n p a m -> VType n p a (Set.fromList ctorNames) m (Map.keysSet m))

createRefType
    :: UIdent
    -> Integer
    -> [Ident]
    -> (Type -> [Type] -> TypeChecker [ObjData])
    -> ([Type] -> IfaceStore)
    -> TypeChecker Type
createRefType = createType (\n p a m -> RType n p a m Map.empty)

iteratorT :: Type -> TypeChecker Type
iteratorT tArg = do
    lookup <- lookupType iteratorI
    t'     <- case lookup of
        Just t' -> return t'
        Nothing -> createValType [iteratorI] iteratorI 1 membIs membOs ifaces
    let t'' = t' { vArgs = [tArg] }
    return t''
  where
    membIs = [iterCurrentI, iterNextI]
    membOs iterT [tVar] = return
        [ Obj (FType iterT tVar)                     False
        , Obj (FType iterT (TupType [boolT, iterT])) False
        ]
    ifaces [tVar] = Map.fromList [(iterableI, Iface iterableI [tVar])]

refIteratorT :: Type -> Type -> TypeChecker Type
refIteratorT tArg1 tArg2 = do
    lookup <- lookupType refIteratorI
    t'     <- case lookup of
        Just t' -> return t'
        Nothing -> createRefType refIteratorI 2 membIs membOs ifaces
    let t'' = t' { rArgs = [tArg1, tArg2] }
    return t''
  where
    membIs = [iterCurrentI, iterNextI]
    membOs iterT [tVar1, tVar2] = return
        [ Obj (FType iterT (FType tVar2 tVar1)) False
        , Obj (FType iterT (FType tVar2 boolT)) False
        ]
    ifaces [tVar1, tVar2] =
        Map.fromList [(refIterableI, Iface refIterableI [tVar1, tVar2])]

iterableT :: TypeChecker Type
iterableT = do
    lookup <- lookupType iterableI
    case lookup of
        Just t' -> return t'
        Nothing -> createValType [iterableI] iterableI 1 membIs membOs ifaces
  where
    membIs = [iterateI]
    membOs iterable [tVar] = do
        iterator <- iteratorT tVar
        return [Obj (FType iterable iterator) False]
    ifaces [tVar] = Map.fromList [(iterableI, Iface iterableI [tVar])]

refIterableT :: TypeChecker Type
refIterableT = do
    lookup <- lookupType refIterableI
    case lookup of
        Just t' -> return t'
        Nothing -> createRefType refIterableI 2 membIs membOs ifaces
  where
    membIs = [iterateI]
    membOs refIterable [tVar1, tVar2] = do
        refIterator <- refIteratorT tVar1 tVar2
        return [Obj (FType refIterable refIterator) False]
    ifaces [tVar1, tVar2] =
        Map.fromList [(refIterableI, Iface refIterableI [tVar1, tVar2])]

iterateI :: Ident
iterateI = Ident "iterate"

iterCurrentI :: Ident
iterCurrentI = Ident "current"

iterNextI :: Ident
iterNextI = Ident "next"

refIterableI :: UIdent
refIterableI = UIdent "RefIterable"

iterableI :: UIdent
iterableI = UIdent "Iterable"

refIteratorI :: UIdent
refIteratorI = UIdent "RefIterator"

iteratorI :: UIdent
iteratorI = UIdent "Iterator"

tParam :: Integer -> Ident
tParam n = Ident ('a' : show n)

tParams :: Integer -> [Ident]
tParams n = map tParam [1 .. n]
