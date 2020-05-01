{-# LANGUAGE FlexibleInstances #-}
module Harper.TypeSystem.Declarations
    ( declTypes
    , Declarable(..)
    , declares
    , declareFromList
    , declIs
    , patDeclIs
    )
where
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Traversable

import           Harper.Abs
import           Harper.Abs.Pos
import           Harper.Abs.Typed
import qualified Harper.Error                  as Error
import           Harper.Output
import           Harper.TypeSystem.Alloc
import           Harper.TypeSystem.Core
import           Harper.TypeSystem.GlobalTypes
import           Harper.TypeSystem.Typing
import           Harper.Utility

getCtorIdents :: TypeDecl Pos -> [UIdent]
getCtorIdents d@(ValTDecl _ (TSig _ i _) _) = [i]
getCtorIdents d@(ValTUDecl _ _ variants) =
    map (\(TVarDecl _ i _) -> i) variants
getCtorIdents d =
    error $ "This type of type declarations is not supported yet: " ++ show d

declTypes :: [TypeDecl Pos] -> TypeChecker ()
declTypes ds = do
    ()   <- assertUniqueNames
    ()   <- assertUniqueCtors
    -- Load empty signatures first so that member declarations see all types.
    sigs <- mapM declToSig ds
    loadTypes (toStore sigs)
    ts <- mapM declToType ds
    loadTypes (toStore ts)
    ctors <- mapM declToCtors ds
    let ctorStore = Map.fromList [ (ctor c, c) | c <- concat ctors ]
    modify (\st -> st { tCtors = ctorStore })
  where
    assertUniqueNames = case findDupsBy toIdent ds of
        ([], []) -> return ()
        (is, ds) -> raise $ Error.conflTypeNames is ds
    assertUniqueCtors =
        case
                findDupsBy
                    fst
                    (concatMap (\d -> zip (getCtorIdents d) (repeat d)) ds)
            of
                ([], []) -> return ()
                (is, ds) -> raise $ Error.conflCtorNames is (map snd ds)
    toIdent d@(ValTDecl  _ (TSig _ i _) _) = uti i
    toIdent d@(ValTUDecl _ (TSig _ i _) _) = uti i
    toIdent d =
        error
            $  "This type of type declarations is not supported yet: "
            ++ show d
    toStore ts = Map.fromList [ (uti i, t) | t@(VType i _ _ _ _) <- ts ]

declToSig :: TypeDecl Pos -> TypeChecker Type
declToSig d@(ValTDecl a sig@(TSig _ i _) body) =
    declToSig (ValTUDecl a sig [TVarDecl a i body])
declToSig d@(ValTUDecl _ sig@(TSig _ i params) _) = do
    let paramIs  = [ i | TParam _ i <- params ]
        typeVars = map TypeVar paramIs
    case findDups paramIs of
        [] -> return $ VType i paramIs typeVars Set.empty Map.empty
        is -> raise $ Error.conflTypeParams is d

declToType :: TypeDecl Pos -> TypeChecker Type
declToType (ValTDecl a sig@(TSig _ i _) body) =
    declToType (ValTUDecl a sig [TVarDecl a i body])
declToType d@(ValTUDecl _ (TSig _ tName _) vs) = do
    t         <- declToSig d
    membTypes <- foldM collectMembs Map.empty vs
    membs     <- mapM (unionMemb (params t)) (Map.toList membTypes)
    return $ t { ctors = Set.fromList (getCtorIdents d)
               , membs = Map.fromList membs
               }
  where
    collectMembs acc (TVarDecl _ ctor body) =
        let membs = case body of
                DataTBody _ _ membs -> membs
                TBody _ membs       -> membs
        in  collectMembs' ctor acc [ th | TMemTHint _ th <- membs ]
    collectMembs' ctor acc membs =
        case findDupsBy (\(THint _ i _) -> i) membs of
            ([], []) ->
                let sigs = map (\th -> (declI th, [th])) membs
                in  return $ Map.unionWith (++) (Map.fromList sigs) acc
            (is, ms) -> raise $ Error.conflMembNames is ctor ms
    unionMemb tParams (i, ths) = do
        ts <- mapM (\(THint _ _ tExpr) -> parseType tExpr) ths
        let boundTs = map (bindVars tParams) ts
        case unifys boundTs of
            Just subst -> return (i, apply subst (head ts))
            Nothing    -> raise $ Error.conflMembTypes tName i ts ths

declToType d =
    error $ "This type of type declarations is not supported yet: " ++ show d

declToCtors :: TypeDecl Pos -> TypeChecker [TypeCtor]
declToCtors (ValTDecl a sig@(TSig _ i _) body) =
    declToCtors (ValTUDecl a sig [TVarDecl a i body])
declToCtors d@(ValTUDecl _ (TSig _ tName params) vs) =
    let ps = [ TypeVar i | TParam _ i <- params ]
    in  mapM (variantToTypeCtor tName ps) vs

variantToTypeCtor
    :: UIdent -> [Type] -> TypeVariantDecl Pos -> TypeChecker TypeCtor
variantToTypeCtor tName vars (TVarDecl _ ctor body) = case body of
    d@(DataTBody _ flds membs) -> do
        let fs = [ f | TFldDecl _ f <- flds ]
        case findDups (map declI fs) of
            [] -> do
                oenv <- mapM declToFld fs
                return $ TypeCtor tName ctor (Map.fromList oenv)
            is -> raise $ Error.conflFldNames is d
    TBody{} -> return $ TypeCtor tName ctor Map.empty
  where
    declToFld (THint _ i texpr) = do
        t <- parseType texpr
        return (i, Obj t False)

class Declarable a where
    declare :: a Pos -> TypeChecker (a (TypeMetaData Pos), OEnv)
    declI :: a b -> Ident

instance Declarable TypeHint where
    declare h@(THint _ i tExpr) = do
        t <- parseType tExpr
        l <- alloc (Obj t False)
        return (annWith t <$> h, Map.fromList [(i, l)])

    declI (THint _ i _) = i

instance Declarable Declaration where
    declare d@(Decl      _ i ) = raise $ Error.locWOutType i d
    declare d@(DeclWHint a th) = do
        (th', env) <- declare th
        return (DeclWHint (annWith (typ th') a) th', env)

    declI (Decl      _ i ) = i
    declI (DeclWHint _ th) = declI th

instance Declarable LocalObjDecl where
    declare (LocVarDecl a decl) = do
        (decl', env) <- declare decl
        forM_ (Map.elems env) markAssignable
        return (LocVarDecl (annWith (typ decl') a) decl', env)
      where
        markAssignable l =
            modifyObjData (Map.adjust (\o -> o { assignable = True }) l)
    declare (LocValDecl a decl) = do
        (decl', env) <- declare decl
        return (LocValDecl (annWith (typ decl') a) decl', env)

    declI (LocVarDecl _ decl) = declI decl
    declI (LocValDecl _ decl) = declI decl

declares
    :: Declarable a => [a Pos] -> TypeChecker ([a (TypeMetaData Pos)], OEnv)
declares as = do
    res <- mapM declare as
    return (map fst res, foldl' (\o (_, o') -> Map.union o' o) Map.empty res)

declareFromList :: [(Ident, Type)] -> TypeChecker OEnv
declareFromList xs = do
    xs' <- mapM declareOne xs
    return $ Map.fromList xs'
  where
    declareOne (i, t) = do
        l <- alloc (Obj t False)
        return (i, l)

declIs :: Declarable a => [a b] -> [Ident]
declIs = map declI

patDeclIs :: Pattern a -> [Ident]
patDeclIs p = case p of
    PatLit  _ _      -> []
    PatDecl _ decl   -> [declI decl]
    PatData _ flds   -> concatMap fldDeclIs flds
    PatDisc _        -> []
    PatCtor _ _ flds -> concatMap fldDeclIs flds
    where fldDeclIs (PatFld _ _ p) = patDeclIs p

parseType :: TypeExpr Pos -> TypeChecker Type
parseType e@(TVar  _ i   ) = return $ TypeVar i
parseType (  TCtor a i   ) = parseType (TApp a i [])
parseType (  TUnit _     ) = return $ PType (UIdent "Unit")
parseType (  TFun _ e1 e2) = do
    p <- parseType e1
    r <- parseType e2
    return $ FType p r
parseType e@(TApp _ u es) = do
    lookup <- getsTypes (Map.lookup $ uti u)
    let n = length es
    case lookup of
        Just t@(VType _ params _ c membs) -> if length params /= n
            then raise $ Error.typeInvArity t (length params) n e
            else do
                tArgs <- mapM parseType es
                let subst = Map.fromList (zip params tArgs)
                return $ VType u params tArgs c (Map.map (apply subst) membs)
        Just t | n == 0 -> return t
        Just t          -> raise $ Error.typeInvArity t 0 n e
        Nothing         -> raise $ Error.undeclaredType u e
parseType (TPur _ (TSideE _)) = return SEType

parseType t =
    error $ "This type of type declarations is not supported: " ++ show t
