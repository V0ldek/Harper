module Harper.Engine
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
import           Data.List

import           Harper.Abs
import           Harper.Printer
import           Harper.Engine.Conditionals
import           Harper.Engine.Error            ( raise )
import qualified Harper.Engine.Error           as Error
import           Harper.Engine.Core
import           Harper.Engine.Expressions
import           Harper.Engine.Snapshots
import           Harper.Engine.Output
import           OutputM

runInterpreter :: Program Pos -> HarperOutput Object
runInterpreter tree =
    evalStateT (runReaderT (interpret tree) Map.empty) Map.empty

interpret :: Program Pos -> Interpreter Object
interpret (Prog _ ds) = do
    let fds = [ fd | TopLvlFDecl _ fd <- ds ]
    env <- decls fds
    local env (eval $ ObjExpr Nothing (Ident "main"))

decls :: [FunDecl Pos] -> Interpreter (Env -> Env)
decls ds = do
    st  <- get
    env <- ask
    let n     = length ds
        ls    = newlocs n st
        is    = [ i | FDecl _ i _ _ <- ds ]
        isls  = zip is ls
        env'  = Map.fromList isls
        fobjs = map (declToFun (Map.union env' env)) ds
        lsfs  = zip ls fobjs
        st'   = Map.fromList lsfs
    modify (Map.union st')
    return (Map.union env')
  where
    declToFun env (FDecl _ i params (FExprBody a body)) =
        case [ i | FArg _ i <- params ] of
            [] -> Thunk body env
            ps -> Fun ps (RetExprStmt a body) env
    declToFun env (FDecl _ i params (FStmtBody _ body)) =
        Fun [ i | FArg _ i <- params ] body env

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

-- Object access.

eval e@(ObjExpr _ i                     ) = do
    lookup <- asks (Map.lookup i)
    case lookup of
        Just l -> do
            o <- gets (Map.! l)
            case o of
                Var (Just (Thunk e env)) -> do
                    o' <- local (const env) (eval e)
                    modify (Map.insert l (Var (Just o')))
                    return o'
                Var Nothing -> raise $ Error.unassVar i e
                Thunk e env -> do
                    o' <- local (const env) (eval e)
                    modify (Map.insert l o')
                    return o'
                Fun [] body env -> local (const env) (call body)
                _               -> return o
        Nothing -> raise $ Error.undeclaredIdent i e

-- Function application.

eval (AppExpr _ e1 e2) = do
    o1 <- eval e1
    apply o1 e2

-- Lambda expressions.

eval (LamExpr _ params body) = do
    env <- ask
    let ps = [ i | LamArg _ (PatDecl _ (LocValDecl _ (Decl _ i))) <- params ]
    return $ Fun ps (fBodyToStmt body) env

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
    argEnv <- ask
    arg    <- makeThunk argV argEnv
    st     <- get
    let l    = newloc st
        env' = Map.insert p l env
    modify (Map.insert l arg)
    case ps of
        [] -> local (const env') (call s)
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
    -> (Object -> Interpreter Object)
    -> Interpreter Object
    -> Interpreter Object
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

-- Declarations.

exec (DeclStmt _ decl) _ k = case decl of
    LocVarDecl _ (Decl _ i) -> do
        st <- get
        let l   = newloc st
            var = Var Nothing
        modify (Map.insert l var)
        local (Map.insert i l) k
exec (DconStmt _ (PatDecl _ decl) e) _ k = do
    env <- ask
    val <- makeThunk e env
    st  <- get
    let l = newloc st
    case decl of
        LocVarDecl _ (Decl _ i) -> do
            let var = Var (Just val)
            modify (Map.insert l var)
            local (Map.insert i l) k
        LocValDecl _ (Decl _ i) -> do
            modify (Map.insert l val)
            local (Map.insert i l) k

-- Assignment.

exec s@(AssStmt _ i e) kRet k = do
    lookup <- asks (Map.lookup i)
    case lookup of
        Just l -> do
            o <- gets (Map.! l)
            case o of
                Var{} -> do
                    env <- ask
                    val <- makeThunk e env
                    modify (Map.insert l (Var $ Just val))
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
        (PUnit   , PUnit   ) -> f $ PUnit == PUnit
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
        (PUnit   , PUnit   ) -> f $ PUnit `compare` PUnit
        _ -> raise $ Error.invCmpTypes (objType o1) (objType o2) e1 e2 e

makeThunk :: Expression Pos -> Env -> Interpreter Object
makeThunk e env = do
    (e', env') <- snapshotExpr e
    return (Thunk e' (env' env))
