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
import           Harper.Abs.Tuple
import           Harper.Abs.Typed
import qualified Harper.Error                  as Error
import           Harper.Output
import           Harper.TypeSystem.Alloc
import           Harper.TypeSystem.Core
import           Harper.TypeSystem.GlobalTypes
import           Harper.TypeSystem.Interfaces
import           Harper.TypeSystem.Typing
import           Harper.Utility

getCtorIdents :: TypeDecl Pos -> [UIdent]
getCtorIdents (ValTDecl _ (TSig _ i _) _) = [i]
getCtorIdents (ValTUDecl _ _ variants) = map (\(TVarDecl _ i _) -> i) variants
getCtorIdents RefTDecl{} = []

declTypes :: [TypeDecl Pos] -> TypeChecker ()
declTypes ds = do
    ()   <- assertUniqueNames
    ()   <- assertUniqueCtors
    -- Load empty signatures first so that member declarations see all types.
    sigs <- mapM declToSig ds
    loadTypes (toStore sigs)
    -- Load ctors to type check fields and find duplicate field names.
    ctors <- mapM declToCtors ds
    -- Now fully parse type declarations to gather member declarations.
    ts    <- mapM declToType ds
    loadTypes (toStore ts)
    let ctorStore = Map.fromList [ (ctor c, c) | c <- concat ctors ]
    modify (\st -> st { tCtors = ctorStore })
    forM_ ts implementInterfaces
    -- Member type signatures now have references to "empty" types without any members or interfaces.
    -- Not the cleanest solution, but it works.
    forM_ ts fixMemberSignatures
  where
    assertUniqueNames = case findDupsBy toIdent ds of
        ([], []) -> return ()
        (is, ds) -> raise $ Error.conflTypeNames is ds
    assertUniqueCtors =
        let isds = concatMap (\d -> zip (getCtorIdents d) (repeat d)) ds
        in  case findDupsBy fst isds of
                ([], []) -> return ()
                (is, ds) -> raise $ Error.conflCtorNames is (map snd ds)
    toIdent d@(ValTDecl  _ (TSig _ i _) _) = uti i
    toIdent d@(ValTUDecl _ (TSig _ i _) _) = uti i
    toIdent d@(RefTDecl  _ (TSig _ i _) _) = uti i
    toStore ts = Map.fromList (map (\t -> (name t, t)) ts)
    fixMemberSignatures VType { vMembs = membs } =
        forM_ (Map.elems membs) fixMemberSignatureAt
    fixMemberSignatures RType { rMembs = membs, rData = flds } = do
        forM_ (Map.elems membs) fixMemberSignatureAt
        forM_ (Map.elems flds)  fixMemberSignatureAt
    fixMemberSignatureAt ptr = do
        o <- gets ((Map.! ptr) . objData)
        t <- fixSignature (objType o)
        modifyObjData (Map.insert ptr o { objType = t })
    fixSignature (VType tName args params _ _ _ _) = do
        t       <- getType tName
        params' <- mapM fixSignature params
        let VType _ _ _ ctors' membs' univMembs' ifaces' = t
        return $ VType tName args params' ctors' membs' univMembs' ifaces'
    fixSignature (RType tName args params membs flds ifaces) = do
        t       <- getType tName
        params' <- mapM fixSignature params
        let RType _ _ _ membs' flds' ifaces' = t
        return $ RType tName args params' membs' flds' ifaces'
    fixSignature (FType p r) = do
        p' <- fixSignature p
        r' <- fixSignature r
        return $ FType p' r'
    fixSignature t = return t

declToSig :: TypeDecl Pos -> TypeChecker Type
declToSig d@(ValTDecl a sig@(TSig _ i _) body) =
    declToSig (ValTUDecl a sig [TVarDecl a i body])
declToSig d@(ValTUDecl _ sig@(TSig _ i params) _) = do
    let paramIs  = [ i | TParam _ i <- params ]
        typeVars = map TypeVar paramIs
    case findDups paramIs of
        [] -> return $ VType i
                             paramIs
                             typeVars
                             Set.empty
                             Map.empty
                             Set.empty
                             Map.empty
        is -> raise $ Error.conflTypeParams is d
declToSig d@(RefTDecl _ sig@(TSig _ i params) _) = do
    let paramIs  = [ i | TParam _ i <- params ]
        typeVars = map TypeVar paramIs
    case findDups paramIs of
        [] -> return $ RType i paramIs typeVars Map.empty Map.empty Map.empty
        is -> raise $ Error.conflTypeParams is d

declToType :: TypeDecl Pos -> TypeChecker Type
declToType (ValTDecl a sig@(TSig _ i _) body) =
    declToType (ValTUDecl a sig [TVarDecl a i body])
declToType d@(ValTUDecl _ (TSig _ tName tParams) vs) = do
    t                    <- declToSig d
    (membTypes, isByVar) <- foldM collectMembs (Map.empty, []) vs
    membs                <- mapM
        (unionMembs t (tVars t))
        (Map.toList (Map.map (map $ addThisArg tName tParams) membTypes))
    ls <- allocs (map (\m -> Obj (snd m) False) membs)
    return $ t { ctors      = Set.fromList (getCtorIdents d)
               , vMembs     = Map.fromList (zip (map fst membs) ls)
               , vUnivMembs = findUniv isByVar
               }
  where
    findUniv []       = Set.empty
    findUniv [x     ] = Set.fromList x
    findUniv (x : xs) = Set.intersection (Set.fromList x) (findUniv xs)
declToType d@(RefTDecl a (TSig a' tName tParams) (TBody a'' membs)) =
    declToType (RefTDecl a (TSig a' tName tParams) (DataTBody a'' [] membs))
declToType d@(RefTDecl _ (TSig _ tName tParams) (DataTBody a'' fldDecls membDecls))
    = do
        t <- declToSig d
        let membThs =
                [ th | TMemTHint _ th <- membDecls, declI th /= ctorIdent ]
            ctorTh =
                [ th | TMemTHint _ th <- membDecls, declI th == ctorIdent ]
            fldThs = [ th | TFldDecl _ th <- fldDecls ]
        () <- assertUniqueMembNames tName (membThs ++ ctorTh)
        () <- assertUniqueFldNames tName fldThs
        let membIsTExprs =
                [ (i, tExpr)
                | THint _ i tExpr <- map (addThisArg tName tParams) membThs
                ]
            ctorIsTExpr = [ (i, tExpr) | THint _ i tExpr <- ctorTh ]
            fldIsTExprs = [ (i, tExpr) | THint _ i tExpr <- fldThs ]
        ctorTExpr <- case ctorTh of
            [THint _ _ tExpr] -> return tExpr
            []                -> raise $ Error.refTypeNoCtor t d
        ctorT  <- parseType ctorTExpr
        ()     <- assertCorrectCtorSig (bindAllVars t) (bindAllVars ctorT)
        membTs <- mapM (parseType . snd) membIsTExprs
        fldTs  <- mapM (parseType . snd) fldIsTExprs
        let membIsTs = zip (map fst membIsTExprs) membTs
            fldIsTs  = zip (map fst fldIsTExprs) fldTs
        ctorLs <- alloc (Obj ctorT False)
        membLs <- allocs (map (\m -> Obj (snd m) False) membIsTs)
        fldLs  <- allocs (map (\f -> Obj (snd f) True) fldIsTs)
        return t
            { rMembs = Map.fromList
                (zip (ctorIdent : map fst membIsTs) (ctorLs : membLs))
            , rData  = Map.fromList (zip (map fst fldIsTs) fldLs)
            }
  where
    assertCorrectCtorSig t ctor = if matchTail t ctor
        then return ()
        else raise $ Error.invCtorType t ctor d
      where
        matchTail t (FType ImpType r) | canUnify t r = True
        matchTail t (FType _ r)                      = matchTail t r
        matchTail _ _                                = False

collectMembs
    :: (Map.Map Ident [TypeHint Pos], [[Ident]])
    -> TypeVariantDecl Pos
    -> TypeChecker (Map.Map Ident [TypeHint Pos], [[Ident]])
collectMembs (acc, is) (TVarDecl _ ctor body) = do
    let ths = case body of
            DataTBody _ flds membs ->
                [ th | TMemTHint _ th <- membs ]
                    ++ [ th | TFldDecl _ th <- flds ]
            TBody _ membs -> [ th | TMemTHint _ th <- membs ]
        is' = map declI ths
    acc' <- collectMembs' ctor acc ths
    return (acc', is' : is)

collectMembs'
    :: UIdent
    -> Map.Map Ident [TypeHint Pos]
    -> [TypeHint Pos]
    -> TypeChecker (Map.Map Ident [TypeHint Pos])
collectMembs' ctor acc membs = do
    () <- assertUniqueMembNames ctor membs
    let sigs = map (\th -> (declI th, [th])) membs
    return $ Map.unionWith (++) (Map.fromList sigs) acc

unionMembs
    :: Type -> [Ident] -> (Ident, [TypeHint Pos]) -> TypeChecker (Ident, Type)
unionMembs t tParams (i, ths) = do
    ts <- mapM (\(THint _ _ tExpr) -> parseType tExpr) ths
    let boundTs = map (bindVars tParams) ts
    case unifys boundTs of
        Just subst -> return (i, apply subst (head ts))
        Nothing    -> raise $ Error.conflMembTypes t i ts ths

addThisArg :: UIdent -> [TypeParameter Pos] -> TypeHint Pos -> TypeHint Pos
addThisArg tName tParams (THint a i tExpr) =
    let thisType = TApp a tName (map (\(TParam _ i) -> TVar a i) tParams)
        tExpr'   = TFun a thisType tExpr
    in  THint a i tExpr'

declToCtors :: TypeDecl Pos -> TypeChecker [TypeCtor]
declToCtors (ValTDecl a sig@(TSig _ i _) body) =
    declToCtors (ValTUDecl a sig [TVarDecl a i body])
declToCtors (ValTUDecl _ (TSig _ tName params) vs) =
    let ps = [ TypeVar i | TParam _ i <- params ]
    in  mapM (variantToTypeCtor tName ps) vs
declToCtors RefTDecl{} = return []

variantToTypeCtor
    :: UIdent -> [Type] -> TypeVariantDecl Pos -> TypeChecker TypeCtor
variantToTypeCtor tName vars (TVarDecl _ ctor body) = case body of
    d@(DataTBody _ flds membs) -> do
        let fs = [ f | TFldDecl _ f <- flds ]
        ()   <- assertUniqueFldNames ctor fs
        oenv <- mapM declToFld fs
        return $ TypeCtor tName ctor (Map.fromList oenv)
    TBody{} -> return $ TypeCtor tName ctor Map.empty
  where
    declToFld (THint _ i texpr) = do
        t <- parseType texpr
        return (i, Obj t False)

assertUniqueMembNames :: UIdent -> [TypeHint Pos] -> TypeChecker ()
assertUniqueMembNames tName ths = case findDupsBy (\(THint _ i _) -> i) ths of
    ([], [] ) -> return ()
    (is, ths) -> raise $ Error.conflMembNames tName is ths

assertUniqueFldNames :: UIdent -> [TypeHint Pos] -> TypeChecker ()
assertUniqueFldNames tName ths = case findDupsBy (\(THint _ i _) -> i) ths of
    ([], [] ) -> return ()
    (is, ths) -> raise $ Error.conflFldNames tName is ths

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
    lookup <- lookupType u
    let n = length es
    case lookup of
        Just t@VType { vParams = params } -> if length params /= n
            then raise $ Error.typeInvArity t (length params) n e
            else do
                tArgs <- mapM parseType es
                let subst = Map.fromList $ zip params tArgs
                return $ apply subst $ t { vName = u }
        Just t@RType { rParams = params } -> if length params /= n
            then raise $ Error.typeInvArity t (length params) n e
            else do
                tArgs <- mapM parseType es
                let subst = Map.fromList $ zip params tArgs
                return $ apply subst $ t { rName = u }
        Just t | n == 0 -> return t
        Just t          -> raise $ Error.typeInvArity t 0 n e
        Nothing         -> raise $ Error.undeclaredType u e
parseType (TTup _ tup) = do
    ts <- mapM parseType $ tTupToList tup
    return $ TupType ts
parseType (TPur _ (TSideE  _)) = return SEType
parseType (TPur _ (TImpure _)) = return ImpType

parseType t =
    error $ "This type of type declarations is not supported: " ++ show t
