module Harper.TypeSystem.GlobalTypes
    ( loadGlobalTypes
    , integerT
    , boolT
    , charT
    , stringT
    , unitT
    , iteratorT
    , refIteratorT
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

iteratorT :: Type -> TypeChecker Type
iteratorT tArg = do
    lookup <- lookupType ident
    t'     <- case lookup of
        Just t' -> return t'
        Nothing -> do
            ls <- newlocs (length membIs)
            let params = tParams 1
                args   = map TypeVar params
                isls   = zip membIs ls
                t'     = VType ident
                               params
                               args
                               (Set.fromList [ident])
                               (Map.fromList isls)
                lsos = zip ls (membOs t' args)
            modifyObjData (Map.union $ Map.fromList lsos)
            loadTypes $ Map.fromList [(ident, t')]
            return t'
    let t'' = t' { vArgs = [tArg] }
    return t''
  where
    membIs = [Ident "current", Ident "next"]
    membOs iterT [tVar] =
        [ Obj (FType iterT tVar)                     False
        , Obj (FType iterT (TupType [boolT, iterT])) False
        ]
    ident = UIdent "Iterator"

refIteratorT :: Type -> Type -> TypeChecker Type
refIteratorT tArg1 tArg2 = do
    lookup <- lookupType ident
    t'     <- case lookup of
        Just t' -> return t'
        Nothing -> do
            ls <- newlocs (length membIs)
            let params = tParams 2
                args   = map TypeVar params
                isls   = zip membIs ls
                t'     = RType ident params args (Map.fromList isls) Map.empty
                lsos   = zip ls (membOs t' args)
            modifyObjData (Map.union $ Map.fromList lsos)
            loadTypes $ Map.fromList [(ident, t')]
            return t'
    let t'' = t' { rArgs = [tArg1, tArg2] }
    return t''
  where
    membIs = [Ident "current", Ident "next"]
    membOs iterT [tVar1, tVar2] =
        [ Obj (FType iterT (FType tVar2 tVar1)) False
        , Obj (FType iterT (FType tVar2 boolT)) False
        ]
    ident = UIdent "RefIterator"

tParam :: Integer -> Ident
tParam n = Ident ('a' : show n)

tParams :: Integer -> [Ident]
tParams n = map tParam [1 .. n]
