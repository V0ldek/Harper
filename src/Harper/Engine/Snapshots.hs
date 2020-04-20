module Harper.Engine.Snapshots
    ( snapshotExpr
    , snapshotStmt
    )
where
import qualified Data.Map                      as Map
import           Control.Monad.Reader
import           Control.Monad.State

import           Harper.Abs
import           Harper.Engine.Core
import           Harper.Engine.Error            ( raise )

snapshotExpr :: Expression Pos -> Interpreter (Expression Pos, Env -> Env)

-- Literals

snapshotExpr e@LitExpr{}     = return (e, id)

-- Object access.

snapshotExpr e@(ObjExpr p i) = do
    lookup <- asks (Map.lookup i)
    case lookup of
        Just l -> do
            o <- gets (Map.! l)
            case o of
                Var (Just t) -> do
                    st  <- get
                    env <- ask
                    let l' = newloc st
                        i' = newvar env
                    modify (Map.insert l' t)
                    return (ObjExpr p i', Map.insert i' l')
                _ -> return (e, id)
        _ -> return (e, id)

-- Function application.

snapshotExpr (AppExpr a e1 e2) = snapshotBinExpr e1 e2 (AppExpr a)

-- Lambda expressions.

snapshotExpr (LamExpr a params body) =
    let ps = [ i | LamArg _ (PatDecl _ (LocValDecl _ (Decl _ i))) <- params ]
    in  local (\env -> foldr Map.delete env ps) snapshotBody
  where
    snapshotBody = case body of
        FExprBody a' e -> do
            (e', env) <- snapshotExpr e
            return (LamExpr a params (FExprBody a' e'), env)
        FStmtBody a' s -> do
            (s', env) <- snapshotStmt s
            return (LamExpr a params (FStmtBody a' s'), env)

-- Integer operators.

snapshotExpr (AddExpr a e1 e2) = snapshotBinExpr e1 e2 (AddExpr a)
snapshotExpr (SubExpr a e1 e2) = snapshotBinExpr e1 e2 (SubExpr a)
snapshotExpr (MulExpr a e1 e2) = snapshotBinExpr e1 e2 (MulExpr a)
snapshotExpr (DivExpr a e1 e2) = snapshotBinExpr e1 e2 (DivExpr a)
snapshotExpr (PowExpr a e1 e2) = snapshotBinExpr e1 e2 (PowExpr a)
snapshotExpr (ModExpr a e1 e2) = snapshotBinExpr e1 e2 (ModExpr a)

snapshotExpr (NegExpr a e    ) = snapshotUnExpr e (NegExpr a)

-- Equations.

snapshotExpr (EqExpr  a e1 e2) = snapshotBinExpr e1 e2 (EqExpr a)
snapshotExpr (NEqExpr a e1 e2) = snapshotBinExpr e1 e2 (NEqExpr a)

-- Comparisons.

snapshotExpr (LEqExpr a e1 e2) = snapshotBinExpr e1 e2 (LEqExpr a)
snapshotExpr (GEqExpr a e1 e2) = snapshotBinExpr e1 e2 (GEqExpr a)
snapshotExpr (LtExpr  a e1 e2) = snapshotBinExpr e1 e2 (LtExpr a)
snapshotExpr (GtExpr  a e1 e2) = snapshotBinExpr e1 e2 (GtExpr a)

-- Boolean operators.
snapshotExpr (AndExpr a e1 e2) = snapshotBinExpr e1 e2 (AndExpr a)
snapshotExpr (OrExpr  a e1 e2) = snapshotBinExpr e1 e2 (OrExpr a)
snapshotExpr (NotExpr a e    ) = snapshotUnExpr e (NotExpr a)

snapshotExpr e                 = error
    ("Evaluating this type of expressions is not implemented yet: " ++ show e)

snapshotUnExpr
    :: Expression Pos
    -> (Expression Pos -> Expression Pos)
    -> Interpreter (Expression Pos, Env -> Env)
snapshotUnExpr e f = do
    (e', env) <- snapshotExpr e
    return (f e, env)

snapshotBinExpr
    :: Expression Pos
    -> Expression Pos
    -> (Expression Pos -> Expression Pos -> Expression Pos)
    -> Interpreter (Expression Pos, Env -> Env)
snapshotBinExpr e1 e2 f = do
    (e1', env1) <- snapshotExpr e1
    (e2', env2) <- local env1 (snapshotExpr e2)
    return (f e1' e2', env2 . env1)

snapshotStmt :: Statement Pos -> Interpreter (Statement Pos, Env -> Env)
snapshotStmt (RetExprStmt a e) = do
    (e', env) <- snapshotExpr e
    return (RetExprStmt a e', env)
snapshotStmt s@EmptyStmt{}    = return (s, id)
snapshotStmt (StmtBlock a ss) = do
    (ss', env) <- foldM combine ([], id) ss
    return (StmtBlock a (reverse ss'), env)
  where
    combine (ss, env) s = do
        (s', env') <- local env (snapshotStmt s)
        return (s' : ss, env' . env)

-- Control flow.

snapshotStmt s@RetStmt{}    = return (s, id)
snapshotStmt (CondStmt a c) = case c of
    IfElifStmts a' _if elifs -> do
        (_if'  , env ) <- snapshotIf _if
        (elifs', env') <- foldM combineElifs ([], env) elifs
        return (CondStmt a (IfElifStmts a' _if' elifs'), env')
    IfElifElseStmts a' _if elifs _else -> do
        (_if'  , env  ) <- snapshotIf _if
        (elifs', env' ) <- foldM combineElifs ([], env) elifs
        (_else', env'') <- snapshotElse _else
        return
            (CondStmt a (IfElifElseStmts a' _if' elifs' _else'), env' . env'')
  where
    snapshotIf (IfStmt a pred s) = do
        (pred', env ) <- snapshotExpr pred
        (s'   , env') <- local env (snapshotStmt s)
        return (IfStmt a pred' s', env' . env)
    snapshotElse (ElseStmt a s) = do
        (s', env) <- snapshotStmt s
        return (ElseStmt a s', env)
    combineElifs (elifs, env) (ElifStmt a pred s) = do
        (pred', env' ) <- local env (snapshotExpr pred)
        (s'   , env'') <- local env' (snapshotStmt s)
        return (ElifStmt a pred' s' : elifs, env'' . env')

-- Declarations.

snapshotStmt s@DeclStmt{}               = return (s, id)
snapshotStmt (DconStmt a p@PatDecl{} e) = do
    (e', env) <- snapshotExpr e
    return (DconStmt a p e', env)

-- Assignment.

snapshotStmt (AssStmt a i e) = snapshotExprStmt e (AssStmt a i)
snapshotStmt (AddStmt a i e) = snapshotExprStmt e (AddStmt a i)
snapshotStmt (SubStmt a i e) = snapshotExprStmt e (SubStmt a i)
snapshotStmt (MulStmt a i e) = snapshotExprStmt e (MulStmt a i)
snapshotStmt (DivStmt a i e) = snapshotExprStmt e (DivStmt a i)
snapshotStmt (PowStmt a i e) = snapshotExprStmt e (PowStmt a i)

snapshotStmt s               = error
    ("Executing this type of expressions is not implemented yet: " ++ show s)

snapshotExprStmt
    :: Expression Pos
    -> (Expression Pos -> Statement Pos)
    -> Interpreter (Statement Pos, Env -> Env)
snapshotExprStmt e f = do
    (e', env) <- snapshotExpr e
    return (f e', env)
