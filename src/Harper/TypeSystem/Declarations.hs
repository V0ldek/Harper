module Harper.TypeSystem.Declarations
    ( typeDecls
    , declare
    )
where
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           Harper.Abs
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

typeDecls :: [TypeDecl Pos] -> TypeChecker (TEnv, CEnv)
typeDecls ds = do
    () <- assertUniqueNames
    () <- assertUniqueCtors
    ts <- mapM declToType ds
    let tenv = Map.fromList [ (Ident i, t) | t@(VType (UIdent i) _ _ _) <- ts ]
    ctors <- localTypes (Map.union tenv) (mapM declToCtors ds)
    let cenv = Map.fromList [ (ctor c, c) | c <- concat ctors ]
    return (tenv, cenv)
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
    toIdent d@(ValTDecl  _ (TSig _ (UIdent i) _) _) = Ident i
    toIdent d@(ValTUDecl _ (TSig _ (UIdent i) _) _) = Ident i
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
                env <- localTypes (Map.union tenv) (declare' fs)
                return $ TypeCtor tName ctor env
    TBody{} -> return $ TypeCtor tName ctor Map.empty

declare :: TypeHint Pos -> TypeChecker OEnv
declare (THint _ i tExpr) = do
    t <- parseType tExpr
    asks (Map.insert i t . objs)

declare' :: [TypeHint Pos] -> TypeChecker OEnv
declare' = foldM combine Map.empty
  where
    combine env f = do
        env' <- declare f
        return $ Map.union env' env

parseType :: TypeExpr Pos -> TypeChecker Type
parseType e@(TVar _ i    ) = do
    lookup <- asks (Map.lookup i . types)
    case lookup of
        Just t  -> return t
        Nothing -> raise $ Error.unboundTypeVar i e
parseType (TUnit _     ) = return $ PType (UIdent "Unit")
parseType (TFun _ e1 e2) = do
    p <- parseType e1
    r <- parseType e2
    return $ FType p r
parseType e@(TCtor _ u@(UIdent i) es) = do
    lookup <- asks (Map.lookup (Ident i) . types)
    let n = length es
    case lookup of
        Just t@(VType _ params _ c) -> if length params /= n
            then raise $ Error.typeInvArity t (length params) n e
            else do
                tArgs <- mapM parseType es
                return $ VType u params tArgs c
        Just t  -> raise $ Error.typeInvArity t 0 n e
        Nothing -> raise $ Error.undeclaredUIdent u e

parseType t =
    error $ "This type of type declarations is not supported: " ++ show t
