-- Some of the code in this module is inspired by
-- "Typing Haskell in Haskell", Mark P. Jones, https://web.cecs.pdx.edu/~mpj/thih/
-- The code is much simpler since Harper doesn't do type reconstruction
-- and has no concept of kinds and all type constructors are always fully applied.
module Harper.TypeSystem.Typing
    ( arity
    , bindVars
    , bindAllVars
    , bindAllVarsInOEnv
    , getFreshValInst
    , getFreshRefInst
    , getMember
    , Subst
    , Types(..)
    , unify
    , unifys
    , canUnify
    , curryType
    , isFunImpure
    )
where
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Maybe

import           Harper.Abs
import           Harper.Error
import           Harper.Output
import           Harper.TypeSystem.Core
import           Harper.Utility

arity :: Type -> Int
arity (FType SEType  r) = arity r
arity (FType ImpType r) = arity r
arity (FType _       r) = 1 + arity r
arity _                 = 0

bindVars :: [Ident] -> Type -> Type
bindVars vars = apply (Map.fromList (map (\i -> (i, TypeBound i)) vars))

bindAllVars :: Type -> Type
bindAllVars t = bindVars (tVars t) t

bindAllVarsInOEnv :: OEnv -> TypeChecker ()
bindAllVarsInOEnv oenv = forM_ (Map.elems oenv) bindOne
  where
    bindOne ptr = modifyObjData
        (Map.adjust (\o -> o { objType = bindAllVars (objType o) }) ptr)

getFreshValInst :: UIdent -> TypeChecker (Maybe (TypeCtor, Type))
getFreshValInst i = do
    luCtor <- getsCtors (Map.lookup i)
    case luCtor of
        Just ctor -> do
            t     <- getType (tname ctor)
            subst <- getFreshTypeSubst t
            return $ Just (apply subst ctor, apply subst t)
        Nothing -> return Nothing

getFreshRefInst :: UIdent -> TypeChecker (Maybe Type)
getFreshRefInst i = do
    lookup <- getsTypes (Map.lookup i)
    case lookup of
        Just t -> do
            subst <- getFreshTypeSubst t
            return $ Just (apply subst t)
        Nothing -> return Nothing

getFreshTypeSubst :: Type -> TypeChecker Subst
getFreshTypeSubst t = do
    let n = length (tVars t)
    fresh <- newvars n
    let subst = Map.fromList $ zip (tVars t) (map TypeVar fresh)
    return subst

getMember :: Type -> Ident -> TypeChecker (Maybe ObjData)
getMember t i = case t of
    VType _ args params _     membs -> getMember' args params membs
    RType _ args params membs _     -> getMember' args params membs
    _                               -> return Nothing
  where
    getMember' :: [Ident] -> [Type] -> OEnv -> TypeChecker (Maybe ObjData)
    getMember' args params membs = case Map.lookup i membs of
        Just ptr -> do
            o <- gets ((Map.! ptr) . objData)
            let subst = Map.fromList $ zip args params
            return $ Just o { objType = apply subst (objType o) }
        Nothing -> return Nothing

type Subst = Map.Map Ident Type

class Types t where
    apply :: Subst -> t -> t
    tVars :: t -> [Ident]

instance Types Type where
    apply s v@(TypeVar i         ) = fromMaybe v (Map.lookup i s)
    apply s v@(VType _ _ args _ _) = v { vArgs = apply s args }
    apply s r@(RType _ _ args _ _) = r { rArgs = apply s args }
    apply s (  FType p r         ) = FType (apply s p) (apply s r)
    apply s (  TupType ts        ) = TupType (map (apply s) ts)
    apply s t                      = t

    tVars (TypeVar i         ) = [i]
    tVars (VType _ _ args _ _) = tVars args
    tVars (RType _ _ args _ _) = tVars args
    tVars (FType p r         ) = tVars p `union` tVars r
    tVars (TupType ts        ) = concatMap tVars ts
    tVars t                    = []

instance Types ObjData where
    apply s (Obj t b) = Obj (apply s t) b

    tVars o = tVars (objType o)

instance Types TypeCtor where
    apply s ctor = ctor { flds = Map.map (apply s) (flds ctor) }

    tVars ctor = foldr (union . tVars) [] (Map.elems $ flds ctor)

instance Types a => Types [a] where
    apply s = map $ apply s
    tVars = nub . concatMap tVars

catSubst :: Subst -> Subst -> Subst
catSubst s1 s2 = Map.union (Map.map (apply s2) s1) s2

-- Unify is left-biased.
unify :: Type -> Type -> Maybe Subst
unify t1 t2 = fromMaybe
    Nothing
    (find
        isJust
        [ unify' t1 t2
        , unify' t1 (FType SEType t2)
        , unify' t1 (FType ImpType t2)
        ]
    )
  where
    unify' (TypeVar i)   t2            = return $ Map.fromList [(i, t2)]
    unify' t1            (TypeVar i  ) = return $ Map.fromList [(i, t1)]
    unify' (FType p1 r1) (FType p2 r2) = do
        s1 <- unify p1 p2
        s2 <- unify (apply s1 r1) (apply s1 r2)
        return $ catSubst s1 s2
    unify' (VType i1 _ ps1 _ _) (VType i2 _ ps2 _ _) | i1 == i2 =
        foldM accSubst Map.empty (zip ps1 ps2)
    unify' (RType i1 _ ps1 _ _) (RType i2 _ ps2 _ _) | i1 == i2 =
        foldM accSubst Map.empty (zip ps1 ps2)
    unify' (TupType ts1) (TupType ts2) = foldM accSubst Map.empty (zip ts1 ts2)
    unify' SEType        ImpType       = return Map.empty
    unify' t1 t2 | t1 == t2            = return Map.empty
    unify' t1 t2                       = Nothing

accSubst :: Subst -> (Type, Type) -> Maybe Subst
accSubst s (p1, p2) = do
    s' <- unify (apply s p1) (apply s p2)
    return $ catSubst s s'

unifys :: [Type] -> Maybe Subst
unifys = accSubst Map.empty
  where
    accSubst s []             = return s
    accSubst s [t           ] = return s
    accSubst s (t1 : t2 : ts) = do
        s' <- unify (apply s t1) (apply s t2)
        accSubst (catSubst s s') ts

canUnify :: Type -> Type -> Bool
canUnify = (isJust .) . unify

curryType :: [Type] -> Type -> Type
curryType ps r = foldr FType r ps

isFunImpure :: Type -> Int -> Bool
isFunImpure _           0 = False
isFunImpure (FType p r) n = isParamImpure p || isFunImpure r (n - 1)
  where
    isParamImpure RType{}                = True
    isParamImpure VType { vArgs = args } = any isParamImpure args
    isParamImpure (FType ImpType r)      = True
    isParamImpure (FType p       r)      = isParamImpure r
    isParamImpure (TupType ts     )      = any isParamImpure ts
    isParamImpure _                      = False
