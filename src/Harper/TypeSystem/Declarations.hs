{-# LANGUAGE FlexibleInstances #-}
module Harper.TypeSystem.Declarations
    ( typeDecls
    , Declarable(..)
    , declareList
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

import           Harper.Abs
import           Harper.Abs.Pos
import           Harper.Abs.Typed
import           Harper.Alloc
import qualified Harper.Error                  as Error
import           Harper.Output
import           Harper.TypeSystem.Core
import           Harper.Utility

getCtorIdents :: TypeDecl Pos -> [UIdent]
getCtorIdents d@(ValTDecl _ (TSig _ i _) _) = [i]
getCtorIdents d@(ValTUDecl _ _ variants) =
    map (\(TVarDecl _ i _) -> i) variants
getCtorIdents d =
    error $ "This type of type declarations is not supported yet: " ++ show d

typeDecls :: [TypeDecl Pos] -> TypeChecker TEnv
typeDecls ds = do
    () <- assertUniqueNames
    () <- assertUniqueCtors
    ts <- mapM declToType ds
    let tenv = Map.fromList [ (uti i, t) | t@(VType i _ _ _) <- ts ]
    ctors <- localTypes (Map.union tenv) (mapM declToCtors ds)
    let cs = Map.fromList [ (ctor c, c) | c <- concat ctors ]
    modify (\st -> st { tCtors = cs })
    return tenv
  where
    assertUniqueNames = case mapFromListUnique (zip (map toIdent ds) ds) of
        Right _           -> return ()
        Left  (i, d1, d2) -> raise $ Error.conflTypeNames i d1 d2
    assertUniqueCtors =
        case
                mapFromListUnique
                    (concatMap (\d -> zip (getCtorIdents d) (repeat d)) ds)
            of
                Right _           -> return ()
                Left  (i, d1, d2) -> raise $ Error.conflCtorNames i d1 d2
    toIdent d@(ValTDecl  _ (TSig _ i _) _) = uti i
    toIdent d@(ValTUDecl _ (TSig _ i _) _) = uti i
    toIdent d =
        error
            $  "This type of type declarations is not supported yet: "
            ++ show d

declToType :: TypeDecl Pos -> TypeChecker Type
declToType (ValTDecl a sig@(TSig _ i _) body) =
    declToType (ValTUDecl a sig [TVarDecl a i body])
declToType d@(ValTUDecl _ (TSig _ tName params) vs) =
    let paramIs  = [ i | TParam _ i <- params ]
        typeVars = map TypeVar paramIs
    in  case setFromListUnique paramIs of
            Right _ -> return $ VType tName
                                      paramIs
                                      typeVars
                                      (Set.fromList (getCtorIdents d))
            Left i -> raise $ Error.conflTypeParam i d
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
    DataTBody _ flds _ -> do
        let tenv = Map.fromList $ zip [ i | TypeVar i <- vars ] vars
        let fs   = [ f | TFldDecl _ f <- flds ]
        case setFromListUnique fs of
            Right _ -> do
                (_, oenv) <- localTypes (Map.union tenv) (declareList fs)
                return $ TypeCtor tName ctor oenv
    TBody{} -> return $ TypeCtor tName ctor Map.empty

class Declarable a where
    declare :: a Pos -> TypeChecker (a (TypeMetaData Pos), OEnv)
    declI :: a b -> Ident

instance Declarable TypeHint where
    declare h@(THint _ i tExpr) = do
        t <- parseType tExpr
        return (annWith t <$> h, Map.fromList [(i, t)])

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
        return (LocVarDecl (annWith (typ decl') a) decl', env)
    declare (LocValDecl a decl) = do
        (decl', env) <- declare decl
        return (LocValDecl (annWith (typ decl') a) decl', env)

    declI (LocVarDecl _ decl) = declI decl
    declI (LocValDecl _ decl) = declI decl

declareList
    :: Declarable a => [a Pos] -> TypeChecker ([a (TypeMetaData Pos)], OEnv)
declareList as = do
    res <- mapM declare as
    return (map fst res, foldl' (\o (_, o') -> Map.union o' o) Map.empty res)

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
    lookup <- asksTypes (Map.lookup $ uti u)
    let n = length es
    case lookup of
        Just t@(VType _ params _ c) -> if length params /= n
            then raise $ Error.typeInvArity t (length params) n e
            else do
                tArgs <- mapM parseType es
                return $ VType u params tArgs c
        Just t | n == 0 -> return t
        Just t          -> raise $ Error.typeInvArity t 0 n e
        Nothing         -> raise $ Error.undeclaredType u e

parseType t =
    error $ "This type of type declarations is not supported: " ++ show t