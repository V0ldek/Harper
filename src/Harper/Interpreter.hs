module Harper.Interpreter
    ( runInterpreter
    , eval
    , apply
    )
where
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Maybe
import           Data.List

import           Harper.Abs
import           Harper.Alloc
import           Harper.Printer
import           Harper.Interpreter.Conditionals
import           Harper.Interpreter.Core
import           Harper.Interpreter.Declarations
import           Harper.Interpreter.Expressions
import           Harper.Interpreter.Snapshots
import           Harper.Interpreter.Thunk
import qualified Harper.Error                  as Error
import           Harper.Output
import           Harper.TypeSystem.Core         ( TypeCtor(..) )
import           OutputM

runInterpreter :: Program Pos -> TEnv -> HarperOutput Object
runInterpreter tree tenv =
    evalStateT (runReaderT (interpret tree) (Env Map.empty tenv)) Map.empty

interpret :: Program Pos -> Interpreter Object
interpret (Prog _ ds) = do
    let fds = [ fd | TopLvlFDecl _ fd <- ds ]
    env <- funDecls fds
    localObjs (const env) (eval $ ObjExpr Nothing (Ident "main"))

fBodyToStmt :: FunBody Pos -> Statement Pos
fBodyToStmt (FStmtBody _ s) = s
fBodyToStmt (FExprBody a e) = RetExprStmt a e

eval :: Expression Pos -> Interpreter Object

-- Literals

eval (  LitExpr _ (IntLit  _ n         )) = return $ PInt n
eval (  LitExpr _ (BoolLit _ (BTrue  _))) = return $ PBool True
eval (  LitExpr _ (BoolLit _ (BFalse _))) = return $ PBool False
eval (  LitExpr _ (StrLit  _ s         )) = return $ PStr s
eval (  LitExpr _ (CharLit _ c         )) = return $ PChar c
eval (  LitExpr _ (UnitLit _           )) = return PUnit

-- Value construction.

eval e@(VCtorExpr _ ctor flds           ) = do
    lookup <- asks (Map.lookup ctor . types)
    case lookup of
        Just t@TypeCtor { flds = fldMap } -> do
            _data <- mapM fldAssToData flds
            let dataIs      = Set.fromList $ map fst _data
                fldIs       = Map.keysSet fldMap
                notDeclared = fldIs Set.\\ dataIs
                excess      = dataIs Set.\\ fldIs
            unless (Set.null notDeclared)
                   (raise $ Error.unassFlds t (Set.toList notDeclared) e)
            unless (Set.null excess)
                   (raise $ Error.excessFlds t (Set.toList excess) e)
            return $ Value t (Map.fromList _data)
        Nothing -> raise $ Error.undeclaredUIdent ctor e
  where
    fldAssToData (DataAss _ i e) = do
        t <- emplaceThunk e
        return (i, fromJust $ this t)

-- Object access.

eval e@(ObjExpr _ i) = do
    lookup <- asks (Map.lookup i . objs)
    case lookup of
        Just l -> do
            o <- gets (Map.! l)
            evalObj o
        Nothing -> raise $ Error.undeclaredIdent i e
  where
    evalObj o = case o of
        Var (Just ptr) -> do
            o' <- gets (Map.! ptr)
            evalObj o'
        Var Nothing     -> raise $ Error.unassVar i e
        Thunk{}         -> evalThunk o
        Fun [] body env -> localObjs (const env) (call body)
        _               -> return o

-- Function application.

eval (AppExpr _ e1 e2) = do
    o1 <- eval e1
    apply o1 e2

 -- Function sequencing.

eval (SeqExpr _ e1 e2) = do
    o1 <- eval e1
    apply o1 e2

-- Lambda expressions.

eval (LamExpr a params body) = do
    env <- asks objs
    let n     = length params
        ps    = newvars n env
        pats  = [ p | LamParam _ p <- params ]
        body' = genMatchChain (zip ps pats) (fBodyToStmt body)
    return $ Fun ps body' env
  where
    genMatchChain ((p, pat) : xs) body = MatchStmt
        a
        (ObjExpr a p)
        [MatchStmtClause a pat (genMatchChain xs body)]
    genMatchChain [] body = body

-- Match expressions.

eval m@(MatchExpr _ e cs) = evalMatches e cs
  where
    evalMatches o (MatchExprClause _ p e : cs) =
        patMatch p o (eval e) (evalMatches o cs)
    evalMatches _ [] = raise $ Error.nonExhPatMatch m

-- Integer operators.

eval e@AddExpr{}      = evalIntBinOp e
eval e@SubExpr{}      = evalIntBinOp e
eval e@MulExpr{}      = evalIntBinOp e
eval e@DivExpr{}      = evalIntBinOp e
eval e@PowExpr{}      = evalIntBinOp e
eval e@ModExpr{}      = evalIntBinOp e

eval e'@(NegExpr a e) = do
    o <- eval e
    case o of
        PInt n -> return $ PInt (-n)
        _      -> raise $ Error.invType (objType o) "Integer" e e'

-- Equations.

eval e@EqExpr{}          = evalEqOp e
eval e@NEqExpr{}         = evalEqOp e

-- Comparisons.

eval e@LEqExpr{}         = evalCmpOp e
eval e@GEqExpr{}         = evalCmpOp e
eval e@LtExpr{}          = evalCmpOp e
eval e@GtExpr{}          = evalCmpOp e

-- Boolean operators.

eval e@(AndExpr _ e1 e2) = do
    o1 <- eval e1
    case o1 of
        PBool b1 -> if not b1
            then return $ PBool False
            else do
                o2 <- eval e2
                case o2 of
                    PBool b2 -> return $ PBool b2
                    _        -> raise $ Error.invType (objType o2) "Bool" e2 e
        _ -> raise $ Error.invType (objType o1) "Bool" e1 e
eval e@(OrExpr _ e1 e2) = do
    o1 <- eval e1
    case o1 of
        PBool b1 -> if b1
            then return $ PBool True
            else do
                o2 <- eval e2
                case o2 of
                    PBool b2 -> return $ PBool b2
                    _        -> raise $ Error.invType (objType o2) "Bool" e2 e
        _ -> raise $ Error.invType (objType o1) "Bool" e1 e
eval e'@(NotExpr _ e) = do
    o <- eval e
    case o of
        PBool b -> return $ PBool $ not b
        _       -> raise $ Error.invType (objType o) "Bool" e e'

eval e =
    error ("Evaluating this type of values is not implemented yet: " ++ show e)

apply :: Object -> Expression Pos -> Interpreter Object
apply (Fun (p : ps) s env) argV = do
    arg <- emplaceThunk argV
    let env' = Map.insert p (fromJust $ this arg) env
    case ps of
        [] -> localObjs (const env') (call s)
        _  -> return $ Fun ps s env'
apply _ argV = raise $ Error.overApp argV

call :: Statement Pos -> Interpreter Object
call s = exec s return f where f = raise $ Error.noReturn s

-- Execution uses continuation-passing-style to implement control flow. 
-- Since statements can only be executed in a body of a function, they take at least two continuations:
-- the "return value" continuation and execution continuation. Calling the kRet short-circuits back to
-- the place of call. Using k continues the execution to the next statement.
exec
    :: Statement Pos
    -> (Object -> Interpreter a)
    -> Interpreter a
    -> Interpreter a
exec (RetExprStmt _ e) kRet _ = do
    o <- eval e
    kRet o
exec (EmptyStmt _         ) _    k = k
exec (StmtBlock _ []      ) _    k = k
exec (StmtBlock a (s : ss)) kRet k = exec s kRet (exec (StmtBlock a ss) kRet k)

-- Control flow.

exec (RetStmt _           ) kRet _ = kRet PUnit
exec (CondStmt _ c) kRet k = let ifs = linearizeCond c in execIfs ifs kRet k
  where
    -- Executes a linear conditional. 
    -- Looks for the first if with a predicate evaluating to true and executes that branch.
    execIfs []                             kRet k = k
    execIfs (s@(IfStmt p pred stmt) : ifs) kRet k = do
        o <- eval pred
        case o of
            PBool True  -> exec stmt kRet k
            PBool False -> execIfs ifs kRet k
            _           -> raise $ Error.invType (objType o) "Bool" pred s
exec w@(WhileStmt _ pred s) kRet k = do
    o <- eval pred
    case o of
        PBool True  -> exec s kRet (exec w kRet k)
        PBool False -> k
        _           -> raise $ Error.invType (objType o) "Bool" pred w
exec m@(MatchStmt _ e cs) kRet k = execMatches e cs
  where
    execMatches e (MatchStmtClause _ p s : cs) =
        patMatch p e (exec s kRet k) (execMatches e cs)
    execMatches _ [] = raise $ Error.nonExhPatMatch m

-- Declarations.

exec (DeclStmt _ decl) _ k = case decl of
    LocVarDecl _ (Decl _ i) -> do
        l <- alloc (Var Nothing)
        localObjs (Map.insert i l) k
exec (DconStmt _ (PatDecl _ decl) e) _ k = do
    val <- emplaceThunk e
    case decl of
        LocVarDecl _ (Decl _ i) -> do
            l <- alloc (Var (this val))
            localObjs (Map.insert i l) k
        LocValDecl _ (Decl _ i) ->
            localObjs (Map.insert i (fromJust $ this val)) k

-- Assignment.

exec s@(AssStmt _ i e) kRet k = do
    lookup <- asks (Map.lookup i . objs)
    case lookup of
        Just l -> do
            o <- gets (Map.! l)
            case o of
                Var{} -> do
                    val <- emplaceThunk e
                    modify (Map.insert l (Var $ this val))
                    k
                Thunk{} -> raise $ Error.assToValue i s
                o       -> raise $ Error.invAss (objType o) i s
        Nothing -> raise $ Error.undeclaredIdent i s
exec (AddStmt p i e) kRet k =
    exec (AssStmt p i (AddExpr p (ObjExpr Nothing i) e)) kRet k
exec (SubStmt p i e) kRet k =
    exec (AssStmt p i (SubExpr p (ObjExpr Nothing i) e)) kRet k
exec (MulStmt p i e) kRet k =
    exec (AssStmt p i (MulExpr p (ObjExpr Nothing i) e)) kRet k
exec (DivStmt p i e) kRet k =
    exec (AssStmt p i (DivExpr p (ObjExpr Nothing i) e)) kRet k
exec (PowStmt p i e) kRet k =
    exec (AssStmt p i (PowExpr p (ObjExpr Nothing i) e)) kRet k

exec s _ _ = error
    ("Executing this type of expressions is not implemented yet: " ++ show s)

evalIntBinOp :: Expression Pos -> Interpreter Object
evalIntBinOp e = do
    let (op, e1, e2, cfz) = case e of
            AddExpr _ e1 e2 -> ((+), e1, e2, False)
            SubExpr _ e1 e2 -> ((-), e1, e2, False)
            MulExpr _ e1 e2 -> ((*), e1, e2, False)
            DivExpr _ e1 e2 -> (div, e1, e2, True)
            PowExpr _ e1 e2 -> ((^), e1, e2, False)
            ModExpr _ e1 e2 -> (mod, e1, e2, True)
    o1 <- eval e1
    o2 <- eval e2
    case (o1, o2) of
        (PInt n1, PInt n2) -> do
            when (cfz && n2 == 0) (raise $ Error.divByZero e2 e)
            return $ PInt $ n1 `op` n2
        (_     , PInt _) -> raise $ Error.invType (objType o1) "Integer" e1 e
        (PInt _, _     ) -> raise $ Error.invType (objType o2) "Integer" e2 e
        (_     , _     ) -> raise
            $ Error.invTypes (objType o1) (objType o2) "Integer" e1 e2 e

evalEqOp :: Expression Pos -> Interpreter Object
evalEqOp e = do
    let (s, e1, e2) = case e of
            EqExpr  _ e1 e2 -> (id, e1, e2)
            NEqExpr _ e1 e2 -> (not, e1, e2)
        f = return . PBool . s
    o1 <- eval e1
    o2 <- eval e2
    case (o1, o2) of
        (PInt  n1, PInt n2 ) -> f $ n1 == n2
        (PBool b1, PBool b2) -> f $ b1 == b2
        (PStr  s1, PStr s2 ) -> f $ s1 == s2
        (PChar c1, PChar c2) -> f $ c1 == c2
        (PUnit   , PUnit   ) -> f True
        _ -> raise $ Error.invEqTypes (objType o1) (objType o2) e1 e2 e

evalCmpOp :: Expression Pos -> Interpreter Object
evalCmpOp e = do
    let (s, e1, e2) = case e of
            LEqExpr _ e1 e2 -> (\o -> o == LT || o == EQ, e1, e2)
            GEqExpr _ e1 e2 -> (\o -> o == GT || o == EQ, e1, e2)
            LtExpr  _ e1 e2 -> ((== LT), e1, e2)
            GtExpr  _ e1 e2 -> ((== GT), e1, e2)
        f = return . PBool . s
    o1 <- eval e1
    o2 <- eval e2
    case (o1, o2) of
        (PInt  n1, PInt n2 ) -> f $ n1 `compare` n2
        (PStr  s1, PStr s2 ) -> f $ s1 `compare` s2
        (PChar c1, PChar c2) -> f $ c1 `compare` c2
        (PUnit   , PUnit   ) -> f EQ
        _ -> raise $ Error.invCmpTypes (objType o1) (objType o2) e1 e2 e

evalThunk :: Object -> Interpreter Object
evalThunk (Thunk e env ptr) = do
    o <- localObjs (const env) (eval e)
    case ptr of
        Just l -> do
            modify (Map.insert l o)
            return o
        _ -> return o
evalThunk o = return o

-- Continuation passing style - kMatch is called when object matches the pattern, kElse otherwise.
patMatch
    :: Pattern Pos
    -> Expression Pos
    -> Interpreter a
    -> Interpreter a
    -> Interpreter a
patMatch PatDisc{}  _ kMatch _     = kMatch
patMatch p@PatLit{} e kMatch kElse = do
    o <- eval e
    patMatch' p o kMatch kElse
patMatch (PatDecl _ decl) e kMatch _ = do
    env <- declLocal decl e
    localObjs (const env) kMatch
patMatch p@PatCtor{} e kMatch kElse = do
    o <- eval e
    patMatch' p o kMatch kElse

patMatch'
    :: Pattern Pos -> Object -> Interpreter a -> Interpreter a -> Interpreter a
patMatch' PatDisc{}      _ kMatch _     = kMatch
patMatch' (PatLit _ lit) o kMatch kElse = do
    o' <- evalThunk o
    case (lit, o') of
        (IntLit _ n1, PInt n2) | n1 == n2   -> kMatch
        (BoolLit _ (BTrue  _), PBool True ) -> kMatch
        (BoolLit _ (BFalse _), PBool False) -> kMatch
        (CharLit _ c1, PChar c2) | c1 == c2 -> kMatch
        (StrLit _ s1, PStr s2) | s1 == s2   -> kMatch
        (UnitLit _, PUnit)                  -> kMatch
        _                                   -> kElse
patMatch' (PatDecl _ decl) o kMatch _ = do
    l   <- alloc o
    env <- declLocal' decl l
    localObjs (const env) kMatch
patMatch' p@(PatCtor _ c flds) o kMatch kElse = do
    o' <- evalThunk o
    case o' of
        v@(Value t _) | ctor t == c -> matchFlds v flds kMatch kElse
        _                           -> kElse
  where
    matchFlds v@(Value t d) (PatFld _ i p : flds) kMatch kElse =
        case Map.lookup i d of
            Just l -> do
                o <- gets (Map.! l)
                patMatch' p o (matchFlds v flds kMatch kElse) kElse
            Nothing -> raise $ Error.invFldAcc t i p
    matchFlds v [] kMatch _ = kMatch
patMatch' p _ _ _ =
    error $ "Pattern matching this type of patterns is unsupported: " ++ show p
