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
import Harper.Engine.Comparable
import Harper.Engine.Object
import ErrM

type Interpreter a = ReaderT Env (StateT Store (Writer String)) a

runInterpreter :: Program -> (Object, String)
runInterpreter tree = runWriter $ evalStateT (runReaderT (interpret tree) Map.empty) Map.empty

interpret :: Program -> Interpreter Object
interpret (Prog ds) = do 
    let fds = [fd | TopLvlFDecl fd <- ds]
    env <- decls fds
    tell "Executing main"
    local env (eval $ ObjVal $ Ident "main")

decls :: [FunDecl] -> Interpreter (Env -> Env)
decls ds = do
    st <- get
    env <- ask
    let n     = length ds
        ls    = newlocs n st
        is    = [i | FDecl i _ _ <- ds]
        isls  = zip is ls
        env'  = Map.fromList isls
        fobjs = map (declToFun (Map.union env' env)) ds
        lsfs  = zip ls fobjs
        st'   = Map.fromList lsfs
    modify (Map.union st')
    return (Map.union env')
    where 
        declToFun e (FDecl i params (FValBody body)) = case [i | FArg i <- params] of 
                                                           [] -> Thunk body e
                                                           ps -> Fun ps body e

eval :: Value -> Interpreter Object
eval l@(LitVal (IntLit n))       = return $ PInt n
eval l@(LitVal (BoolLit BTrue))  = return $ PBool True
eval l@(LitVal (BoolLit BFalse)) = return $ PBool False
eval l@(LitVal (StrLit s))       = return $ PStr s
eval l@(LitVal (CharLit c))      = return $ PChar c

eval (ObjVal i) = do
    tell $ "\nEvaluating " ++ show i
    l <- asks (Map.! i)
    o <- gets (Map.! l)
    case o of
        Thunk v e -> do
            o' <- local (Map.union e) (eval v)
            modify (Map.insert l o')
            tell $ "\nWas a thunk, evaluated into:   " ++ show o'
            return o'
        _ -> do
            tell $ "\nEvaluated into:   " ++ show o
            return o

eval (AppVal v1 v2) = do
    o1 <- eval v1
    apply o1 v2

eval (AddVal v1 v2)     = evalAddOp v1 v2
eval (SubVal v1 v2)     = evalSubOp v1 v2
eval (MulVal v1 v2)     = evalMulOp v1 v2
eval (DivVal v1 v2)     = evalDivOp v1 v2
eval (PowVal v1 v2)     = evalPowOp v1 v2
eval (ModVal v1 v2)     = evalModOp v1 v2
eval (EqVal v1 v2)      = evalEqOp v1 v2
eval (NEqVal v1 v2)     = evalNEqOp v1 v2
eval (LEqVal v1 v2)     = evalLEqOp v1 v2
eval (GEqVal v1 v2)     = evalGEqOp v1 v2
eval (LessVal v1 v2)    = evalLessOp v1 v2
eval (GreaterVal v1 v2) = evalGreaterOp v1 v2
eval (AndVal v1 v2)     = evalAndOp v1 v2
eval (OrVal v1 v2)      = evalOrOp v1 v2
eval (NotVal v)         = evalNotOp v

apply :: Object -> Value -> Interpreter Object
apply (Fun (p:ps) v e) argV = do
    st <- get
    env <- ask
    let l  = newloc st
        e' = Map.insert p l e
    modify (Map.insert l (Thunk argV env))
    case ps of
        [] -> local (Map.union e') (eval v)
        _  -> return $ Fun ps v e'
apply _ _ = error "Applied to too many arguments"

-- OPERATORS

evalAddOp :: Value -> Value -> Interpreter Object
evalSubOp :: Value -> Value -> Interpreter Object
evalMulOp :: Value -> Value -> Interpreter Object
evalDivOp :: Value -> Value -> Interpreter Object
evalPowOp :: Value -> Value -> Interpreter Object
evalModOp :: Value -> Value -> Interpreter Object

evalAddOp = evalIntBinOp (+)
evalSubOp = evalIntBinOp (-)
evalMulOp = evalIntBinOp (*)
evalDivOp = evalIntBinOp div
evalPowOp = evalIntBinOp (^)
evalModOp = evalIntBinOp mod

evalIntBinOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Interpreter Object
evalIntBinOp f v1 v2 = do
    o1 <- eval v1
    o2 <- eval v2
    let n1 = intValue o1
        n2 = intValue o2
    return $ PInt $ f n1 n2

evalEqOp :: Value -> Value -> Interpreter Object
evalNEqOp :: Value -> Value -> Interpreter Object
evalLessOp :: Value -> Value -> Interpreter Object
evalGreaterOp :: Value -> Value -> Interpreter Object
evalLEqOp :: Value -> Value -> Interpreter Object
evalGEqOp :: Value -> Value -> Interpreter Object
evalEqOp      = evalCmpOp (==)
evalNEqOp     = evalCmpOp (/=)
evalLessOp    = evalCmpOp (<)
evalGreaterOp = evalCmpOp (>)
evalLEqOp     = evalCmpOp (<=)
evalGEqOp     = evalCmpOp (>=)

evalCmpOp :: (ComparableObject -> ComparableObject -> Bool) -> Value -> Value -> Interpreter Object
evalCmpOp f v1 v2 = do
    o1 <- eval v1
    o2 <- eval v2
    let cmp1 = CmpObj o1
        cmp2 = CmpObj o2
    return $ PBool $ f cmp1 cmp2

evalNotOp :: Value -> Interpreter Object
evalNotOp v = do
    o <- eval v
    case o of
        PBool b -> return $ PBool $ not b
        _       -> error "Expected Bool value to not operator."

evalAndOp :: Value -> Value -> Interpreter Object
evalOrOp :: Value -> Value -> Interpreter Object
evalAndOp v1 v2 = do
    o1 <- eval v1
    case o1 of
        PBool b1 -> if not b1 then return $ PBool False
                    else do
                        o2 <- eval v2
                        case o2 of
                            PBool b2 -> return $ PBool b2
                            _        -> error "Expected Bool value to and operator."
        _        -> error "Expected Bool value to and operator."
evalOrOp v1 v2 = do
    o1 <- eval v1
    case o1 of
        PBool b1 -> if b1 then return $ PBool True
                    else do
                        o2 <- eval v2
                        case o2 of
                            PBool b2 -> return $ PBool b2
                            _        -> error "Expected Bool value to or operator."
        _        -> error "Expected Bool value to or operator."