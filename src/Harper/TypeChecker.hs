module Harper.TypeChecker
    ( runTypeChecker
    )
where
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Maybe

import           Harper.Abs
import           Harper.Abs.Pos
import qualified Harper.Error                  as Error
import           Harper.Output
import           Harper.Printer                 ( Print(..) )
import           Harper.TypeSystem.Alloc
import           Harper.TypeSystem.Core
import           Harper.TypeSystem.Declarations
import           Harper.TypeSystem.GlobalDeclarations
import           Harper.TypeSystem.GlobalTypes
import           Harper.TypeSystem.StaticAnalysis
import           Harper.TypeSystem.Traits
import           Harper.TypeSystem.Typing
import           Harper.Abs.Typed
import           Harper.Utility

runTypeChecker :: Program Pos -> HarperOutput (Program (TypeMetaData Pos))
runTypeChecker tree = evalStateT
    (runReaderT (typeCheck tree) (Env Map.empty))
    (St (BlkSt True [] False) 0 Map.empty Map.empty Map.empty)

typeCheck :: Program Pos -> TypeChecker (Program (TypeMetaData Pos))
typeCheck p@(Prog a ds) = do
    let tds = [ td | TopLvlTDecl _ td <- ds ]
        fts = [ ft | TopLvlTHint _ ft <- ds ]
        fs  = [ f | TopLvlFDecl _ f <- ds ]
    loadTypes globalTypes
    declTypes tds
    globalEnv       <- declareGlobals
    (fts', userEnv) <- declares fts
    let env = Env $ Map.union userEnv globalEnv
    fs'   <- mapM (local (const env) . annotate) fs
    tds'  <- mapM (local (const env) . annotateTypeMembers) tds
    ctors <- gets tCtors
    return (toProg tds' fts' fs')
  where
    toProg tds fts fs =
        let tds' = [ TopLvlTDecl (annWith unitT $ pos td) td | td <- tds ]
            fts' = [ TopLvlTHint (annWith (typ th) a) th | th <- fts ]
            fs'  = [ TopLvlFDecl (annWith unitT $ pos f) f | f <- fs ]
        in  Prog (annWith unitT a) (tds' ++ fts' ++ fs')
    annotateTypeMembers (ValTDecl a' sig@(TSig _ tName _) body) = do
        body' <- annotateTypeBody tName body
        return $ ValTDecl (annWith unitT a') (annWith unitT <$> sig) body'
    annotateTypeMembers (ValTUDecl a' sig@(TSig _ tName _) variants) = do
        variants' <- mapM
            (\(TVarDecl a i body) -> do
                body' <- annotateTypeBody tName body
                return $ TVarDecl (annWith unitT a) i body'
            )
            variants
        return $ ValTUDecl (annWith unitT a') (annWith unitT <$> sig) variants'
    annotateTypeBody tName (TBody a membs) = do
        membs' <- mapM (annotateMemb tName) membs
        return $ TBody (annWith unitT a) membs'
    annotateTypeBody tName (DataTBody a flds membs) = do
        membs' <- mapM (annotateMemb tName) membs
        return $ DataTBody (annWith unitT a)
                           (map (annWith unitT <$>) flds)
                           membs'
    annotateMemb _ th@TMemTHint{} = return $ annWith unitT <$> th
    annotateMemb tName (TMemFDecl a decl@(FDecl a' i params body)) = do
        t <- getType tName
        let t'      = bindAllVars t
            params' = FParam Nothing thisIdent : params
            membIs  = Map.keys (membs t')
        membsData <- mapM (getMember t') membIs
        let membTypes = map ((\(Obj t _) -> t) . fromJust) membsData
            membs     = zip membIs membTypes
        oenv  <- declareFromList membs
        decl' <- localObjs (Map.union oenv) (annotate (FDecl a' i params' body))
        return $ TMemFDecl (annWith unitT a) decl'

annotate :: FunDecl Pos -> TypeChecker (FunDecl (TypeMetaData Pos))
annotate d@(FDecl a i params body) = do
    lookup <- lookupObj i
    case lookup of
        Just Obj { objType = t@FType{} } ->
            let (n1, n2) = (arity t, length params)
            in
                if n1 < n2
                    then raise $ Error.tooManyParams i t n1 n2 d
                    else do
                        let t'@(FType p r) = bindAllVars t
                        (params', tBody) <- uncurryType t' params
                        paramEnv         <- declareFromList
                            [ (i, typ p) | p@(FParam _ i) <- params' ]
                        (body', bodySt) <- localObjs (Map.union paramEnv)
                                                     (annotateBody body)
                        let params'' = funParams params' bodySt
                            t''      = curryType (map typ params'') (typ body')
                        if canUnify t' t''
                            then return
                                $ FDecl (annWith unitT a) i params'' body'
                            else raise $ Error.funInvType i t t'' d
        Just o | null params -> do
            (body', bodySt) <- annotateBody body
            let t      = objType o
                t'     = bindAllVars t
                params = funParams [] bodySt
                t''    = curryType (map typ params) (typ body')
            if canUnify t' t''
                then return $ FDecl (annWith unitT a) i params body'
                else raise $ Error.funInvType i t t'' d
        Just o  -> raise $ Error.nonFunDeclWithParams i (objType o) d
        Nothing -> raise $ Error.funWOutType i d
  where
    uncurryType (FType tParam r) (FParam a i : ps) = do
        let param' = FParam (tParam, a) i
        (params', t') <- uncurryType r ps
        return (param' : params', t')
    uncurryType t [] = return ([], t)
    uncurryType t ps =
        error
            $  "uncurryType called with nonf type "
            ++ show t
            ++ " and non empty param list "
            ++ show ps
            ++ ". Arity check fail."
    curryType ps r = foldr combine r ps
      where
        combine SEType r@(FType SEType _) = r
        combine p      r                  = FType p r

annotateExpr :: Expression Pos -> TypeChecker (Expression (TypeMetaData Pos))

-- Literals

annotateExpr (LitExpr a l) =
    let l' = annotateLit l in return $ LitExpr (annWith (typ l') a) l'

-- Value construction.

annotateExpr e@(VCtorExpr a ctor fldAss) = do
    tInst <- getFreshInst ctor
    case tInst of
        Just (t@(TypeCtor tName _ flds), t') -> do
            let assMap = Map.fromList [ (i, d) | d@(DataAss _ i _) <- fldAss ]
                fldIs  = Map.keysSet flds
                assIs  = Map.keysSet assMap
                notAss = fldIs Set.\\ assIs
                excess = assIs Set.\\ fldIs
            unless (Set.null notAss)
                   (raise $ Error.unassFlds t (Set.toList notAss) e)
            unless (Set.null excess)
                   (raise $ Error.excessFlds t (Set.toList excess) e)
            fldRes <- mapM annotateFld fldAss
            let (fldAss', substs) = unzip fldRes
                ts                = map (`apply` t') substs
                subst             = unifys ts
            case subst of
                Just s ->
                    let t'' = if null ts then t' else apply s (head ts)
                    in  return $ VCtorExpr (annWith (apply s t'') a)
                                           ctor
                                           fldAss'
                Nothing -> raise $ Error.conflFldSubsts t' t ts e
          where
            annotateFld (DataAss a i e') = do
                e'' <- annotateExpr e'
                let t  = objType $ flds Map.! i
                    t' = typ e''
                case unify t' t of
                    Just subst -> return (DataAss (annWith t a) i e'', subst)
                    Nothing    -> raise $ Error.invType t' t e' e
        Nothing -> raise $ Error.undeclaredCtor ctor e

-- Object access.

annotateExpr e@(ObjExpr a i) = do
    lookup <- lookupObj i
    case lookup of
        Just o  -> return $ ObjExpr (annWith (objType o) a) i
        Nothing -> raise $ Error.undeclaredIdent i e

-- Function application.

annotateExpr e@(AppExpr a e1 e2) = do
    (a', e1', e2') <- annotateAppExpr e a e1 e2
    return $ AppExpr a' e1' e2'

 -- Function sequencing.

annotateExpr e@(SeqExpr a e1 e2) = do
    (a', e1', e2') <- annotateAppExpr e a e1 e2
    return $ SeqExpr a' e1' e2'

-- Lambda expressions.

annotateExpr e@(LamExpr a params body) = do
    (params', oenv  ) <- annotateParams
    (body'  , bodySt) <- localObjs (Map.union oenv) (annotateBody body)
    let params'' = lamParams params' bodySt
        t        = curryType (map typ params'') (typ body')
    return $ LamExpr (annWith t a) params'' body'
  where
    annotateParams = do
        let fldIs = concatMap (\(LamParam _ pat) -> patDeclIs pat) params
        case findDups fldIs of
            [] -> do
                paramPats <- mapM annotateParam params
                let (params', envs) = unzip paramPats
                    env             = foldr Map.union Map.empty envs
                return (params', env)
            ds -> raise $ Error.conflPatDecls ds e
    annotateParam (LamParam a pat) = do
        (pat', oenv) <- annotatePat pat
        return (LamParam (annWith (typ pat') a) pat', oenv)
    curryType ps r = foldr FType r ps

-- Match expressions.

annotateExpr m@(MatchExpr a e cs) = do
    e'  <- annotateExpr e
    cs' <- mapM (annotateMatchExprClause (typ e')) cs
    let typs = map typ cs' -- Typs is nonempty, since the grammar disallows clauseless match expressions.
        s    = unifys typs
    case s of
        Just subst ->
            return $ MatchExpr (annWith (apply subst (head typs)) a) e' cs'
        Nothing -> raise $ Error.conflMatchClauseTypes typs m

-- Integer operators.

annotateExpr e@(AddExpr a e1 e2) = do
    (a', e1', e2') <- annotateBinExpr integerT e a e1 e2
    return $ AddExpr a' e1' e2'
annotateExpr e@(SubExpr a e1 e2) = do
    (a', e1', e2') <- annotateBinExpr integerT e a e1 e2
    return $ SubExpr a' e1' e2'
annotateExpr e@(MulExpr a e1 e2) = do
    (a', e1', e2') <- annotateBinExpr integerT e a e1 e2
    return $ MulExpr a' e1' e2'
annotateExpr e@(DivExpr a e1 e2) = do
    (a', e1', e2') <- annotateBinExpr integerT e a e1 e2
    return $ DivExpr a' e1' e2'
annotateExpr e@(PowExpr a e1 e2) = do
    (a', e1', e2') <- annotateBinExpr integerT e a e1 e2
    return $ PowExpr a' e1' e2'
annotateExpr e@(ModExpr a e1 e2) = do
    (a', e1', e2') <- annotateBinExpr integerT e a e1 e2
    return $ ModExpr a' e1' e2'

annotateExpr e@(NegExpr a e') = do
    (a', e'') <- annotateUnExpr integerT e a e'
    return $ NegExpr a' e''

-- Equations.

annotateExpr e@(EqExpr a e1 e2) = do
    (a', e1', e2') <- annotateEqExpr e a e1 e2
    return $ EqExpr a' e1' e2'
annotateExpr e@(NEqExpr a e1 e2) = do
    (a', e1', e2') <- annotateEqExpr e a e1 e2
    return $ NEqExpr a' e1' e2'

-- Comparisons.

annotateExpr e@(LEqExpr a e1 e2) = do
    (a', e1', e2') <- annotateCmpExpr e a e1 e2
    return $ LEqExpr a' e1' e2'
annotateExpr e@(GEqExpr a e1 e2) = do
    (a', e1', e2') <- annotateCmpExpr e a e1 e2
    return $ GEqExpr a' e1' e2'
annotateExpr e@(LtExpr a e1 e2) = do
    (a', e1', e2') <- annotateCmpExpr e a e1 e2
    return $ LtExpr a' e1' e2'
annotateExpr e@(GtExpr a e1 e2) = do
    (a', e1', e2') <- annotateCmpExpr e a e1 e2
    return $ GtExpr a' e1' e2'

-- Boolean operators.

annotateExpr e@(AndExpr a e1 e2) = do
    (a', e1', e2') <- annotateBinExpr boolT e a e1 e2
    return $ AndExpr a' e1' e2'
annotateExpr e@(OrExpr a e1 e2) = do
    (a', e1', e2') <- annotateBinExpr boolT e a e1 e2
    return $ OrExpr a' e1' e2'

annotateExpr e@(NotExpr a e') = do
    (a', e'') <- annotateUnExpr boolT e a e'
    return $ NotExpr a' e''

-- Member access.

annotateExpr e@(MembExpr a e' acc) = do
    e''       <- annotateExpr e'
    (t, acc') <- annotateAccess (typ e'') e acc
    return $ MembExpr (annWith t a) e'' (reverse acc')

annotateExpr e@(TMembExpr _ _ []) =
    error
        $ "Type member access with an empty access list. Grammar should disallow this."
        ++ show e
annotateExpr e@(TMembExpr a tName (MembAcc a' i : as)) = do
    luType <- lookupType tName
    case luType of
        Just t@(VType _ _ _ _ membs) -> do
            luMemb <- getMember t i
            case luMemb of
                Just (Obj t' _) -> do
                    (t'', as') <- annotateAccess t' e as
                    return $ TMembExpr (annWith t'' a)
                                       tName
                                       (MembAcc (annWith t'' a') i : as')
                Nothing -> raise $ Error.invMembAccess t i e
        Just t  -> raise $ Error.invMembAccess t i e
        Nothing -> raise $ Error.undeclaredType tName e

-- This identifier.

annotateExpr e@(ThisExpr a) = do
    lookup <- lookupObj thisIdent
    case lookup of
        Just o  -> return $ ThisExpr (annWith (objType o) a)
        Nothing -> raise $ Error.thisOutsideOfMember e

annotateExpr e =
    error $ "This type of expressions is not supported yet: " ++ show e

annotateAccess
    :: (Print p, Position p)
    => Type
    -> p
    -> [MemberAccess Pos]
    -> TypeChecker (Type, [MemberAccess (TypeMetaData Pos)])
annotateAccess t ctx = foldM access (t, [])
  where
    access (t@(VType tName _ _ _ membs), acc') (MembAcc a i) = do
        luMemb <- getMember t i
        case luMemb of
            Just (Obj (FType p r) _) | canUnify p t ->
                return (r, MembAcc (annWith r a) i : acc')
            Just (Obj t' _) ->
                error
                    $  "The type"
                    ++ show tName
                    ++ " has a member "
                    ++ show i
                    ++ " whose first formal parameter is not of that type. This should be impossible."
            Nothing -> raise $ Error.invMembAccess t i ctx
    access (t, _) (MembAcc _ i) = raise $ Error.invMembAccess t i ctx

annotateLit :: Literal Pos -> Literal (TypeMetaData Pos)
annotateLit l@IntLit{}  = annWith integerT <$> l
annotateLit l@BoolLit{} = annWith boolT <$> l
annotateLit l@StrLit{}  = annWith stringT <$> l
annotateLit l@CharLit{} = annWith charT <$> l
annotateLit l@UnitLit{} = annWith unitT <$> l

annotateUnExpr
    :: Type
    -> Expression Pos
    -> Pos
    -> Expression Pos
    -> TypeChecker (TypeMetaData Pos, Expression (TypeMetaData Pos))
annotateUnExpr t e a e' = do
    e'' <- annotateExpr e'
    let t' = typ e''
    if canUnify t t'
        then return (annWith t a, e'')
        else raise $ Error.invType t' t e' e

annotateBinExpr
    :: Type
    -> Expression Pos
    -> Pos
    -> Expression Pos
    -> Expression Pos
    -> TypeChecker
           ( TypeMetaData Pos
           , Expression (TypeMetaData Pos)
           , Expression (TypeMetaData Pos)
           )
annotateBinExpr t e a e1 e2 = do
    e1' <- annotateExpr e1
    e2' <- annotateExpr e2
    let t1 = typ e1'
        t2 = typ e2'
    if canUnify t t1 && canUnify t t2
        then return (annWith t a, e1', e2')
        else raise $ Error.invTypes t1 t2 t e1 e2 e

annotateEqExpr
    :: Expression Pos
    -> Pos
    -> Expression Pos
    -> Expression Pos
    -> TypeChecker
           ( TypeMetaData Pos
           , Expression (TypeMetaData Pos)
           , Expression (TypeMetaData Pos)
           )
annotateEqExpr e a e1 e2 = do
    e1' <- annotateExpr e1
    e2' <- annotateExpr e2
    let t1 = typ e1'
        t2 = typ e2'
    if equatable t1 t2
        then return (annWith boolT a, e1', e2')
        else raise $ Error.invEqTypes t1 t2 e1 e2 e

annotateCmpExpr
    :: Expression Pos
    -> Pos
    -> Expression Pos
    -> Expression Pos
    -> TypeChecker
           ( TypeMetaData Pos
           , Expression (TypeMetaData Pos)
           , Expression (TypeMetaData Pos)
           )
annotateCmpExpr e a e1 e2 = do
    e1' <- annotateExpr e1
    e2' <- annotateExpr e2
    let t1 = typ e1'
        t2 = typ e2'
    if comparable t1 t2
        then return (annWith boolT a, e1', e2')
        else raise $ Error.invCmpTypes t1 t2 e1 e2 e

annotateAppExpr
    :: Expression Pos
    -> Pos
    -> Expression Pos
    -> Expression Pos
    -> TypeChecker
           ( TypeMetaData Pos
           , Expression (TypeMetaData Pos)
           , Expression (TypeMetaData Pos)
           )
annotateAppExpr e a e1 e2 = do
    e1' <- annotateExpr e1
    e2' <- annotateExpr e2
    let t1 = typ e1'
        t2 = typ e2'
    case t1 of
        FType p r -> case unify p t2 of
            Just subst -> return (annWith (apply subst r) a, e1', e2')
            Nothing | p == SEType && t2 == unitT -> case e2 of
                LitExpr _ (UnitLit _) -> do
                    sideeffect
                    return (annWith r a, e1', e2')
                _ -> raise $ Error.sideeffectNotUnitLit e2 e
            Nothing -> raise $ Error.invType t2 p e2 e
        _ -> raise $ Error.invApp t1 e

annotateMatchExprClause
    :: Type
    -> MatchExpressionClause Pos
    -> TypeChecker (MatchExpressionClause (TypeMetaData Pos))
annotateMatchExprClause t c@(MatchExprClause a p e) = do
    (p', oenv) <- annotatePat p
    let t' = typ p'
    case unify t t' of
        Just s -> do
            e' <- localObjs (Map.union oenv) (annotateExpr e)
            return $ MatchExprClause (annWith (typ e') a) p' e'
        Nothing -> raise $ Error.patInvType t' t c

annotatePat :: Pattern Pos -> TypeChecker (Pattern (TypeMetaData Pos), OEnv)
annotatePat (PatDisc a) = do
    var <- newvar
    return (PatDisc (annWith (TypeVar var) a), Map.empty)
annotatePat p@(PatLit a l) = do
    let l' = annotateLit l
        t' = typ l'
    return (PatLit (annWith t' a) l', Map.empty)
annotatePat p@(PatDecl a decl) = do
    (decl', oenv) <- declare decl
    let t' = bindAllVars (typ decl')
    bindAllVarsInOEnv oenv
    return (PatDecl (annWith t' a) decl', oenv)
annotatePat p@(PatCtor a i flds) = do
    cInst <- getFreshInst i
    case cInst of
        Just (ctor@(TypeCtor tName _ _), t) -> do
            fldPats <- mapM (annotateFldPattern ctor) flds
            let declIs = concatMap (\(PatFld _ _ pat) -> patDeclIs pat) flds
            case findDups declIs of
                [] ->
                    let (flds', substs, envs) = unzip3 fldPats
                        env                   = foldr Map.union Map.empty envs
                        ts                    = map (`apply` t) substs
                        subst                 = unifys ts
                    in  case subst of
                            Just s ->
                                let t' = if null ts
                                        then t
                                        else apply s (head ts)
                                in  return (PatCtor (annWith t' a) i flds', env)
                            Nothing -> raise $ Error.conflFldSubsts t ctor ts p
                ds -> raise $ Error.conflPatDecls ds p
        Nothing -> raise $ Error.undeclaredCtor i p
  where
    annotateFldPattern ctor@(TypeCtor tName _ ctorFlds) e@(PatFld a i p') =
        case Map.lookup i ctorFlds of
            Just fld -> do
                (p'', env) <- annotatePat p'
                let t'  = objType fld
                    t'' = typ p''
                case unify t' t'' of
                    Just subst ->
                        return
                            ( PatFld (annWith (apply subst t'') a) i p''
                            , subst
                            , env
                            )
                    Nothing -> raise $ Error.patInvType t'' t' e
            Nothing -> raise $ Error.invFldAcc ctor i p

annotateBody
    :: FunBody Pos -> TypeChecker (FunBody (TypeMetaData Pos), BlockState)
annotateBody body = case body of
    FExprBody a e -> do
        e' <- annotateExpr e
        return (FExprBody (annWith (typ e') a) e', mempty)
    FStmtBody a s -> do
        (s', t, bodySt) <- annotateStmtBody s
        return (FStmtBody (annWith t a) s', bodySt)

annotateStmtBody
    :: Statement Pos
    -> TypeChecker (Statement (TypeMetaData Pos), Type, BlockState)
annotateStmtBody s = do
    clearBlkSt
    (s', bodySt) <- blockScope (analStmt s)
    t            <- unifyRets (reachable bodySt) (rets bodySt)
    return (s', t, bodySt)
  where
    unifyRets reachable []
        | reachable
        = return unitT
        | otherwise
        = error
            $ "End of function body is unreachable but there are no return types. This should be impossible: "
            ++ show s
    unifyRets reachable rets = case unifys rets of
        Just subst ->
            let ret = apply subst (head rets)
            in  if reachable && ret /= unitT
                    then raise $ Error.noRet s
                    else return ret
        Nothing -> raise $ Error.conflRetTypes rets s

analStmt :: Statement Pos -> TypeChecker (Statement (TypeMetaData Pos))
analStmt (EmptyStmt a         ) = return $ EmptyStmt (annWith unitT a)
analStmt (StmtBlock a []      ) = return $ StmtBlock (annWith unitT a) []
analStmt (StmtBlock a (s : ss)) = case s of
    (DeclStmt a decl) -> do
        (decl', oenv) <- declare decl
        blk           <- localObjs (Map.union oenv) (analStmt $ StmtBlock a ss)
        let StmtBlock _ ss' = blk
        return $ StmtBlock (annWith unitT a)
                           (DeclStmt (annWith unitT a) decl' : ss')
    (DconStmt a pat e) -> do
        (pat', oenv) <- annotatePat pat
        e'           <- annotateExpr e
        let t1 = typ pat'
            t2 = typ e'
        if canUnify t1 t2
            then do
                blk <- localObjs (Map.union oenv) (analStmt (StmtBlock a ss))
                let StmtBlock _ ss' = blk
                return $ StmtBlock
                    (annWith unitT a)
                    (DconStmt (annWith unitT a) pat' e' : ss')
            else raise $ Error.patInvType t1 t2 pat
    _ -> do
        s'  <- analStmt s
        blk <- analStmt (StmtBlock a ss)
        let StmtBlock _ ss' = blk
        return $ StmtBlock (annWith unitT a) (s' : ss')

-- Control flow.

analStmt (RetExprStmt a e) = do
    e' <- annotateExpr e
    let t = typ e'
    addRet t
    unreachable
    return $ RetExprStmt (annWith t a) e'
analStmt (RetStmt a) = do
    addRet unitT
    unreachable
    return $ RetStmt (annWith unitT a)
analStmt (CondStmt a (IfElifStmts a' _if elifs)) = do
    (if', afterIf) <- blockScope (analIf _if)
    elifRes        <- mapM (blockScope . analElif) elifs
    let (elifs', afterElifs) = unzip elifRes
    mayEnterOneOf (afterIf : afterElifs)
    return $ CondStmt (annWith unitT a)
                      (IfElifStmts (annWith unitT a') if' elifs')
analStmt (CondStmt a (IfElifElseStmts a' _if elifs _else)) = do
    (if', afterIf)     <- blockScope (analIf _if)
    elifRes            <- mapM (blockScope . analElif) elifs
    (else', afterElse) <- blockScope (analElse _else)
    let (elifs', afterElifs) = unzip elifRes
    mustEnterOneOf (afterIf : afterElse : afterElifs)
    return $ CondStmt (annWith unitT a)
                      (IfElifElseStmts (annWith unitT a') if' elifs' else')
analStmt w@(WhileStmt a e s) = do
    e' <- annotateExpr e
    let t = typ e'
    if predicate t
        then do
            (s', after) <- blockScope (analStmt s)
            mayEnterOneOf [after]
            return $ WhileStmt (annWith unitT a) e' s'
        else raise $ Error.invPredType t e w
analStmt m@(MatchStmt a e cs) = do
    e'        <- annotateExpr e
    clauseRes <- mapM (blockScope . annotateMatchStmtClause (typ e')) cs
    let (cs', afters) = unzip clauseRes
        typs          = map typ cs' -- Typs is nonempty, since the grammar disallows clauseless match statements.
        subst         = unifys typs
    -- One of the match clauses will always be entered (or a non exhaustive pattern error will be thrown).
    mustEnterOneOf afters
    case subst of
        Just s  -> return $ MatchStmt (annWith (apply s (head typs)) a) e' cs'
        Nothing -> raise $ Error.conflMatchClauseTypes typs e

-- Declarations.

analStmt d@DeclStmt{} =
    error
        $ "Declaration statement outside of a block. Grammar should disallow this:"
        ++ show d
analStmt d@DconStmt{} =
    error
        $ "Deconstruction statement outside of a block. Grammar should disallow this:"
        ++ show d

-- Assignment.

analStmt s@(AssStmt a i e) = do
    e' <- analAss i e s
    return $ AssStmt (annWith unitT a) i e'
analStmt s@(AddStmt a i e) = do
    e' <- analAss i e s
    return $ AddStmt (annWith unitT a) i e'
analStmt s@(SubStmt a i e) = do
    e' <- analAss i e s
    return $ SubStmt (annWith unitT a) i e'
analStmt s@(MulStmt a i e) = do
    e' <- analAss i e s
    return $ MulStmt (annWith unitT a) i e'
analStmt s@(DivStmt a i e) = do
    e' <- analAss i e s
    return $ DivStmt (annWith unitT a) i e'
analStmt s@(PowStmt a i e) = do
    e' <- analAss i e s
    return $ PowStmt (annWith unitT a) i e'

analStmt (EvalStmt a e) = do
    e' <- annotateExpr e
    return (EvalStmt (annWith unitT a) e')

analStmt s = error $ "This type of statements is not supported yet: " ++ show s

analAss
    :: (Print p, Position p)
    => Ident
    -> Expression Pos
    -> p
    -> TypeChecker (Expression (TypeMetaData Pos))
analAss i e ctx = do
    lookup <- lookupObj i
    case lookup of
        Just o | assignable o -> do
            e' <- annotateExpr e
            let t  = objType o
                t' = typ e'
            if canUnify t t'
                then return e'
                else raise $ Error.invType t' t e ctx
        Just o  -> raise $ Error.assToValue i ctx
        Nothing -> raise $ Error.undeclaredIdent i ctx

annotateMatchStmtClause
    :: Type
    -> MatchStatementClause Pos
    -> TypeChecker (MatchStatementClause (TypeMetaData Pos))
annotateMatchStmtClause t c@(MatchStmtClause a p s) = do
    (p', oenv) <- annotatePat p
    let t' = typ p'
    env <- ask
    if canUnify t t'
        then do
            let vars = patDeclIs p
            s' <- localObjs (Map.union oenv) (analStmt s)
            return $ MatchStmtClause (annWith unitT a) p' s'
        else raise $ Error.patInvType t' t c

analIf :: IfStatement Pos -> TypeChecker (IfStatement (TypeMetaData Pos))
analIf c@(IfStmt a e s) = do
    e' <- annotateExpr e
    let t = typ e'
    if predicate t
        then do
            s' <- analStmt s
            return $ IfStmt (annWith unitT a) e' s'
        else raise $ Error.invPredType t e c

analElif
    :: ElseIfStatement Pos -> TypeChecker (ElseIfStatement (TypeMetaData Pos))
analElif c@(ElifStmt a e s) = do
    e' <- annotateExpr e
    let t = typ e'
    if predicate t
        then do
            s' <- analStmt s
            return $ ElifStmt (annWith unitT a) e' s'
        else raise $ Error.invPredType t e c

analElse :: ElseStatement Pos -> TypeChecker (ElseStatement (TypeMetaData Pos))
analElse c@(ElseStmt a s) = do
    s' <- analStmt s
    return $ ElseStmt (annWith unitT a) s'

funParams
    :: [FunParam (TypeMetaData Pos)]
    -> BlockState
    -> [FunParam (TypeMetaData Pos)]
funParams ps st | hasSideeffects st = case mLast ps of
    Just (FParam (SEType, _) _) -> ps
    _                           -> ps ++ [FParam (SEType, Nothing) (Ident "()")]
funParams ps _ = ps

lamParams
    :: [LambdaParam (TypeMetaData Pos)]
    -> BlockState
    -> [LambdaParam (TypeMetaData Pos)]
lamParams ps st | hasSideeffects st = case mLast ps of
    Just (LamParam (SEType, _) _) -> ps
    _ -> ps ++ [LamParam (SEType, Nothing) (PatDisc (SEType, Nothing))]
lamParams ps _ = ps


