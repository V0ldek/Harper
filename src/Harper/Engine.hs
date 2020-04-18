module Harper.Engine (
    runInterpreter,
    eval,
    apply
)
where
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map as Map
import Data.List

import Harper.Abs
import Harper.Printer
import Harper.Engine.Conditionals
import qualified Harper.Engine.Error as Error
import Harper.Engine.Object
import Harper.Engine.Values
import Harper.Engine.Output
import OutputM

type Interpreter a = ReaderT Env (StateT Store (Output ShowS)) a

runInterpreter :: Program Pos -> HarperOutput Object
runInterpreter tree = evalStateT (runReaderT (interpret tree) Map.empty) Map.empty

raise :: HarperOutput a -> Interpreter a
raise = lift . lift

interpret :: Program Pos -> Interpreter Object
interpret (Prog _ ds) = do 
    let fds = [fd | TopLvlFDecl _ fd <- ds]
    env <- decls fds
    local env (eval $ ObjVal Nothing (Ident "main"))

decls :: [FunDecl Pos] -> Interpreter (Env -> Env)
decls ds = do
    st <- get
    env <- ask
    let n     = length ds
        ls    = newlocs n st
        is    = [i | FDecl _ i _ _ <- ds]
        isls  = zip is ls
        env'  = Map.fromList isls
        fobjs = map (declToFun (Map.union env' env)) ds
        lsfs  = zip ls fobjs
        st'   = Map.fromList lsfs
    modify (Map.union st')
    return (Map.union env')
    where 
        declToFun e (FDecl _ i params (FValBody a body)) = case [i | FArg _ i <- params] of 
                                                           [] -> Thunk body e
                                                           ps -> Fun ps (RetValStmt a body) e
        declToFun e (FDecl _ i params (FStmtBody _ body)) = Fun [i | FArg _ i <- params] body e

fBodyToStmt :: FunBody Pos -> Statement Pos
fBodyToStmt (FStmtBody _ s) = s
fBodyToStmt (FValBody a v) = RetValStmt a v

eval :: Value Pos -> Interpreter Object
eval (LitVal _ (IntLit _ n))           = return $ PInt n
eval (LitVal _ (BoolLit _ (BTrue _)))  = return $ PBool True
eval (LitVal _ (BoolLit _ (BFalse _))) = return $ PBool False
eval (LitVal _ (StrLit _ s))           = return $ PStr s
eval (LitVal _ (CharLit _ c))          = return $ PChar c
eval (UnitVal _)                       = return PUnit

eval (ObjVal _ i) = do
    let Ident s = i
    l <- asks (Map.! i)
    o <- gets (Map.! l)
    case o of
        Thunk v e -> do
            o' <- local (Map.union e) (eval v)
            modify (Map.insert l o')
            return o'
        Fun [] body e -> local (Map.union e) (call body)
        _ -> return o

eval (AppVal _ v1 v2) = do
    o1 <- eval v1
    apply o1 v2

eval (LamVal _ params body) = do
    env <- ask
    let ps = [i | LamArg _ (PatDecl _ (LocVDecl _ (LocSVal _) (Decl _ i))) <- params]
    return $ Fun ps (fBodyToStmt body) env

eval v@AddVal {} = evalIntBinOp v
eval v@SubVal {} = evalIntBinOp v
eval v@MulVal {} = evalIntBinOp v
eval v@DivVal {} = evalIntBinOp v
eval v@PowVal {} = evalIntBinOp v
eval v@ModVal {} = evalIntBinOp v

eval v@EqVal {}  = evalEqOp v
eval v@NEqVal {} = evalEqOp v

eval v@LEqVal {}     = evalCmpOp v
eval v@GEqVal {}     = evalCmpOp v
eval v@LessVal {}    = evalCmpOp v
eval v@GreaterVal {} = evalCmpOp v

eval v@(AndVal _ v1 v2) = do
    o1 <- eval v1
    case o1 of
        PBool b1 -> if not b1 then return $ PBool False
                    else do
                        o2 <- eval v2
                        case o2 of
                            PBool b2 -> return $ PBool b2
                            _        -> raise $ Error.invType (objType o2) "Bool" v2 v
        _        ->  raise $ Error.invType (objType o1) "Bool" v1 v
eval v@(OrVal _ v1 v2) = do
    o1 <- eval v1
    case o1 of
        PBool b1 -> if b1 then return $ PBool True
                    else do
                        o2 <- eval v2
                        case o2 of
                            PBool b2 -> return $ PBool b2
                            _        -> raise $ Error.invType (objType o2) "Bool" v2 v
        _        -> raise $ Error.invType (objType o1) "Bool" v1 v
eval v'@(NotVal _ v)         = do
    o <- eval v
    case o of
        PBool b -> return $ PBool $ not b
        _       -> raise $ Error.invType (objType o) "Bool" v v'

eval v = error ("Evaluating this type of values is not implemented yet: " ++ show v)

apply :: Object -> Value Pos -> Interpreter Object
apply (Fun (p:ps) s e) argV = do
    st <- get
    env <- ask
    let l  = newloc st
        e' = Map.insert p l e
    modify (Map.insert l (Thunk argV env))
    case ps of
        [] -> local (Map.union e') (call s)
        _  -> return $ Fun ps s e'
apply _ argV = raise $ Error.overApp argV

call :: Statement Pos -> Interpreter Object
call s = exec s return f
    where f = raise $ Error.noReturn s

-- Execution uses continuation-passing-style to implement control flow. 
-- Since statements can only be executed in a body of a function, they take at least two continuations:
-- the "return value" continuation and execution continuation. Calling the kRet short-circuits back to
-- the place of call. Using k continues the execution to the next statement.
exec :: Statement Pos -> (Object -> Interpreter Object) -> Interpreter Object -> Interpreter Object
exec (RetValStmt _ v) kRet _ = do
    o <- eval v
    kRet o
exec (RetStmt _) kRet _ = kRet PUnit
exec (EmptyStmt _) _ k = k
exec (StmtBlock _ []) _ k = k
exec (StmtBlock a (s:ss)) kRet k = exec s kRet (exec (StmtBlock a ss) kRet k)
exec (CondStmt _ c) kRet k = let ifs = linearizeCond c in execIfs ifs kRet k
exec s _ _ = error ("Executing this type of statements is not implemented yet: " ++ show s)

-- Executes a linear conditional. 
-- Looks for the first if with a predicate evaluating to true and executes that branch.
execIfs :: [IfStatement Pos] -> (Object -> Interpreter Object) -> Interpreter Object -> Interpreter Object
execIfs [] kRet k = k
execIfs (s@(IfStmt p pred stmt):ifs) kRet k = do
    b <- eval pred
    case b of
        PBool True  -> exec stmt kRet k
        PBool False -> execIfs ifs kRet k
        _           -> raise $ Error.invType (objType b) "Bool" pred s

evalIntBinOp :: Value Pos  -> Interpreter Object
evalIntBinOp v = do
    let (op, v1, v2, cfz) = case v of
                                AddVal _ v1 v2 -> ((+), v1, v2, False)
                                SubVal _ v1 v2 -> ((-), v1, v2, False)
                                MulVal _ v1 v2 -> ((*), v1, v2, False)
                                DivVal _ v1 v2 -> (div, v1, v2, True)
                                PowVal _ v1 v2 -> ((^), v1, v2, False)
                                ModVal _ v1 v2 -> (mod, v1, v2, True)
    o1 <- eval v1
    o2 <- eval v2
    case (intValue o1, intValue o2) of
        (Nothing, Nothing) -> raise $ Error.invTypes (objType o1) (objType o2) "Integer" v1 v2 v
        (Nothing, _)       -> raise $ Error.invType (objType o1) "Integer" v1 v
        (_, Nothing)       -> raise $ Error.invType (objType o2) "Integer" v2 v
        (Just n1, Just n2) -> do
            when (cfz && n2 == 0) (raise $ Error.divByZero v2 v)
            return $ PInt $ n1 `op` n2

evalEqOp :: Value Pos -> Interpreter Object
evalEqOp v = do
    let (s, v1, v2) = case v of
                          EqVal _ v1 v2 ->  (id, v1, v2)
                          NEqVal _ v1 v2 -> (not, v1, v2)
        f = return . PBool . s
    o1 <- eval v1
    o2 <- eval v2
    case (o1, o2) of
        (PInt n1, PInt n2)   -> f $ n1 == n2
        (PBool b1, PBool b2) -> f $ b1 == b2
        (PStr s1, PStr s2)   -> f $ s1 == s2
        (PChar c1, PChar c2) -> f $ c1 == c2
        (PUnit, PUnit)       -> f $ PUnit == PUnit
        _                    -> raise $ Error.invEqTypes (objType o1) (objType o2) v1 v2 v

evalCmpOp :: Value Pos -> Interpreter Object
evalCmpOp v = do
    let (s, v1, v2) = case v of
                          LEqVal _ v1 v2 -> (\o -> o == LT || o == EQ, v1, v2)
                          GEqVal _ v1 v2 -> (\o -> o == GT || o == EQ, v1, v2)
                          LessVal _ v1 v2 ->  ((== LT), v1, v2)
                          GreaterVal _ v1 v2 -> ((== GT), v1, v2)
        f = return . PBool . s
    o1 <- eval v1
    o2 <- eval v2
    case (o1, o2) of
        (PInt n1, PInt n2)   -> f $ n1 `compare` n2
        (PStr s1, PStr s2)   -> f $ s1 `compare` s2
        (PChar c1, PChar c2) -> f $ c1 `compare` c2
        (PUnit, PUnit)       -> f $ PUnit `compare` PUnit
        _                    -> raise $ Error.invCmpTypes (objType o1) (objType o2) v1 v2 v
