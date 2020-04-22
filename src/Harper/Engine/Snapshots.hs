module Harper.Engine.Snapshots
    ( snapshotExpr
    )
where
import qualified Data.Map                      as Map
import           Control.Monad.Reader
import           Control.Monad.State

import           Harper.Abs
import           Harper.Engine.Alloc
import           Harper.Engine.Core
import           Harper.Engine.Error            ( raise )

snapshotExpr :: Expression Pos -> Interpreter (Expression Pos, OEnv -> OEnv)

-- Literals

snapshotExpr e@LitExpr{}             = return (e, id)

-- Value construction.

snapshotExpr (VCtorExpr a ctor flds) = do
    (flds', env) <- foldM combine ([], id) flds
    return (VCtorExpr a ctor (reverse flds'), env)
  where
    combine (flds, env) (DataAss a i e) = do
        (e', env') <- snapshotExpr e
        return (DataAss a i e' : flds, env' . env)

-- Object access.

snapshotExpr e@(ObjExpr p i) = do
    lookup <- asks (Map.lookup i . objs)
    case lookup of
        Just l -> do
            o <- gets (Map.! l)
            case o of
                Var (Just t) -> do
                    l'  <- copy t
                    env <- asks objs
                    let i' = newvar env
                    return (ObjExpr p i', Map.insert i' l')
                _ -> return (e, id)
        _ -> return (e, id)

-- Function application.

snapshotExpr (AppExpr a e1 e2) = snapshotBinExpr e1 e2 (AppExpr a)

-- Function sequencing.

snapshotExpr (SeqExpr a e1 e2) = snapshotBinExpr e1 e2 (SeqExpr a)

-- Lambda expressions.

snapshotExpr e@LamExpr{} = return (e, id)

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
    -> Interpreter (Expression Pos, OEnv -> OEnv)
snapshotUnExpr e f = do
    (e', env) <- snapshotExpr e
    return (f e, env)

snapshotBinExpr
    :: Expression Pos
    -> Expression Pos
    -> (Expression Pos -> Expression Pos -> Expression Pos)
    -> Interpreter (Expression Pos, OEnv -> OEnv)
snapshotBinExpr e1 e2 f = do
    (e1', env1) <- snapshotExpr e1
    (e2', env2) <- localObjs env1 (snapshotExpr e2)
    return (f e1' e2', env2 . env1)

copy :: Ptr -> Interpreter Ptr
copy l = do
    o <- gets (Map.! l)
    alloc o