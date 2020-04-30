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
import           Harper.Interpreter.Alloc
import           Harper.Printer
import           Harper.Interpreter.Conditionals
import           Harper.Interpreter.Core
import           Harper.Interpreter.Declarations
import           Harper.Expressions
import           Harper.Interpreter.Thunk
import qualified Harper.Error                  as Error
import           Harper.Output
import           Harper.TypeSystem.Core         ( TypeCtor(..) )
import           Harper.TypeSystem.GlobalTypes
import           Harper.Abs.Typed
import           OutputM

runInterpreter :: Program Meta -> TEnv -> HarperOutput ShowS
runInterpreter tree tenv =
    evalStateT (runReaderT (interpret tree) (Env Map.empty tenv)) (St Map.empty 0)

interpret :: Program Meta -> Interpreter ShowS
interpret (Prog _ ds) = do
    let fds = [ fd | TopLvlFDecl _ fd <- ds ]
    globals <- nativeObjs
    userEnv <- localObjs (const globals) (funDecls fds call)
    let env = Map.union userEnv globals
    o <- localObjs (const env) runMain
    printObj o

runMain :: Interpreter Object
runMain = do
    lookup <- asksObjs (Map.lookup (Ident "main"))
    case lookup of
        Just ptr -> do
            main <- getsObjs (Map.! ptr)
            case main of
                Fun []           body env -> localObjs (const env) body
                Fun [Ident "()"] body env -> localObjs (const env) body
                --Thunk e env _ -> localObjs (const env) (eval e)
                _                         -> raise Error.invMainType
        Nothing -> raise Error.undeclaredMain

fBodyToStmt :: FunBody Meta -> Statement Meta
fBodyToStmt (FStmtBody _ s) = s
fBodyToStmt (FExprBody a e) = RetExprStmt a e

eval :: Expression Meta -> Interpreter Object

-- Literals

eval (  LitExpr _ (IntLit  _ n         )) = return $ PInt n
eval (  LitExpr _ (BoolLit _ (BTrue  _))) = return $ PBool True
eval (  LitExpr _ (BoolLit _ (BFalse _))) = return $ PBool False
eval (  LitExpr _ (StrLit  _ s         )) = return $ PStr s
eval (  LitExpr _ (CharLit _ c         )) = return $ PChar c
eval (  LitExpr _ (UnitLit _           )) = return PUnit

-- Value construction.

eval e@(VCtorExpr _ ctor flds           ) = do
    lookup <- asksTypes (Map.lookup ctor)
    case lookup of
        Just t@TypeCtor { flds = fldMap } -> do
            _data <- mapM fldAssToData flds
            return $ Value t (Map.fromList _data)
        Nothing ->
            error
                $  "Undeclared ctor. Type check should've caught this."
                ++ show e
  where
    fldAssToData (DataAss _ i e) = do
        (ptr, _) <- makeThunk e eval
        return (i, ptr)

-- Object access.

eval e@(ObjExpr _ i) = do
    lookup <- asksObjs (Map.lookup i)
    case lookup of
        Just l -> do
            o <- getsObjs (Map.! l)
            evalObj o
        Nothing ->
            error
                $  "Undeclared ident. Type check should've caught this."
                ++ show e
  where
    evalObj o = case o of
        Var (Just ptr) -> do
            o' <- getsObjs (Map.! ptr)
            evalObj o'
        Var Nothing     -> raise $ Error.unassVar i e
        Thunk t         -> t
        Fun [] body env -> localObjs (const env) body
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
    ps <- newvars n 
    let pats  = [ p | LamParam _ p <- params ]
        body' = genMatchChain (zip ps pats) (fBodyToStmt body)
    return $ Fun ps (call body') env
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
        _ ->
            error
                $ "Invalid type in negation. Type check should have caught this. "
                ++ show e'

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
                    _ ->
                        error
                            $ "Invalid type in and expression. Type check should have caught this. "
                            ++ show e
        _ ->
            raise
                $ error
                $ "Invalid type in and expression. Type check should have caught this. "
                ++ show e
eval e@(OrExpr _ e1 e2) = do
    o1 <- eval e1
    case o1 of
        PBool b1 -> if b1
            then return $ PBool True
            else do
                o2 <- eval e2
                case o2 of
                    PBool b2 -> return $ PBool b2
                    _ ->
                        error
                            $ "Invalid type in or expression. Type check should have caught this. "
                            ++ show e
        _ ->
            error
                $ "Invalid type in or expression. Type check should have caught this. "
                ++ show e
eval e'@(NotExpr _ e) = do
    o <- eval e
    case o of
        PBool b -> return $ PBool $ not b
        _ ->
            error
                $ "Invalid type in not expression. Type check should have caught this. "
                ++ show e

eval e =
    error ("Evaluating this type of values is not implemented yet: " ++ show e)

apply :: Object -> Expression Meta -> Interpreter Object
apply (Fun (p : ps) s env) argV = do
    (ptr, _) <- makeThunk argV eval
    let env' = Map.insert p ptr env
    case ps of
        [] -> localObjs (const env') s
        _  -> return $ Fun ps s env'
apply (Fun [] s env) (LitExpr _ (UnitLit _)) = localObjs (const env) s
apply f argV =
    error
        $  "Overapplication at runtime. Type check should have caught this. "
        ++ show argV
        ++ " "
        ++ show f

call :: Statement Meta -> Interpreter Object
call s = exec s return f where f = return PUnit

-- Execution uses continuation-passing-style to implement control flow. 
-- Since statements can only be executed in a body of a function, they take at least two continuations:
-- the "return value" continuation and execution continuation. Calling the kRet short-circuits back to
-- the place of call. Using k continues the execution to the next statement.
exec
    :: Statement Meta
    -> (Object -> Interpreter a)
    -> Interpreter a
    -> Interpreter a
exec (EmptyStmt _   ) _    k = k
exec (StmtBlock a ss) kRet k = do
    k'    <- inCurrentScope k
    kRet' <- inCurrentScope2 kRet
    execSeq ss kRet' k'
  where
    execSeq []       kRet k = k
    execSeq (s : ss) kRet k = exec s kRet (execSeq ss kRet k)

-- Control flow.

exec (RetExprStmt _ e) kRet _ = do
    o <- eval e
    kRet o
exec (RetStmt _   ) kRet _ = kRet PUnit
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
            _ ->
                error
                    $ "Invalid type of if predicate. Type check should have caught this. "
                    ++ show c
exec w@(WhileStmt _ pred s) kRet k = do
    o <- eval pred
    case o of
        PBool True  -> exec s kRet (exec w kRet k)
        PBool False -> k
        _ ->
            error
                $ "Invalid type of while predicate. Type check should have caught this. "
                ++ show w
exec m@(MatchStmt _ e cs) kRet k = execMatches e cs
  where
    execMatches e (MatchStmtClause _ p s : cs) =
        patMatch p e (exec s kRet k) (execMatches e cs)
    execMatches _ [] = raise $ Error.nonExhPatMatch m

-- Declarations.

exec (DeclStmt _ decl) _ k = do
    oenv <- declLocalUnass decl
    localObjs (const oenv) k
exec d@(DconStmt _ pat e) _ k =
    patMatch pat e k (raise $ Error.nonExhPatMatch d)

-- Assignment.

exec s@(AssStmt _ i e) kRet k = do
    lookup <- asksObjs (Map.lookup i)
    case lookup of
        Just l -> do
            o <- getsObjs (Map.! l)
            case o of
                Var{} -> do
                    (ptr, _) <- makeThunk e eval
                    modifyObjs (Map.insert l (Var $ Just ptr))
                    k
                o ->
                    error
                        $ "Invalid assignment. Type check should've caught this."
                        ++ show s
        Nothing ->
            error
                $  "Undeclared ident. Type check should've caught this."
                ++ show s
exec (AddStmt p i e) kRet k =
    exec (AssStmt p i (AddExpr p (ObjExpr (typ p, Nothing) i) e)) kRet k
exec (SubStmt p i e) kRet k =
    exec (AssStmt p i (SubExpr p (ObjExpr (typ p, Nothing) i) e)) kRet k
exec (MulStmt p i e) kRet k =
    exec (AssStmt p i (MulExpr p (ObjExpr (typ p, Nothing) i) e)) kRet k
exec (DivStmt p i e) kRet k =
    exec (AssStmt p i (DivExpr p (ObjExpr (typ p, Nothing) i) e)) kRet k
exec (PowStmt p i e) kRet k =
    exec (AssStmt p i (PowExpr p (ObjExpr (typ p, Nothing) i) e)) kRet k

exec (EvalStmt _ e) kRet k = do
    _ <- eval e
    k

exec s _ _ =
    error
        ("Executing this type of statements is not implemented yet: " ++ show s)

evalIntBinOp :: Expression Meta -> Interpreter Object
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
        _ ->
            error
                $ "Invalid type in binary integer operator. Type check should have caught this. "
                ++ show e

evalEqOp :: Expression Meta -> Interpreter Object
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
        _ ->
            error
                $ "Invalid types in eq operator. Type check should have caught this. "
                ++ show e

evalCmpOp :: Expression Meta -> Interpreter Object
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
        _ ->
            error
                $ "Invalid types in cmp operator. Type check should have caught this. "
                ++ show e

evalThunk :: Object -> Interpreter Object
evalThunk (Thunk x) = x
evalThunk o         = return o

-- Continuation passing style - kMatch is called when object matches the pattern, kElse otherwise.
patMatch
    :: Pattern Meta
    -> Expression Meta
    -> Interpreter a
    -> Interpreter a
    -> Interpreter a
patMatch PatDisc{}  _ kMatch _     = kMatch
patMatch p@PatLit{} e kMatch kElse = do
    o <- eval e
    patMatch' p o kMatch kElse
patMatch (PatDecl _ decl) e kMatch _ = do
    env <- declLocal decl e eval
    localObjs (const env) kMatch
patMatch p@PatCtor{} e kMatch kElse = do
    o <- eval e
    patMatch' p o kMatch kElse

patMatch'
    :: Pattern Meta -> Object -> Interpreter a -> Interpreter a -> Interpreter a
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
                o <- getsObjs (Map.! l)
                patMatch' p o (matchFlds v flds kMatch kElse) kElse
            Nothing ->
                error
                    $  "Invalid fld access. Type check should've caught this."
                    ++ show p
    matchFlds v [] kMatch _ = kMatch
patMatch' p _ _ _ =
    error $ "Pattern matching this type of patterns is unsupported: " ++ show p

printObj :: Object -> Interpreter ShowS
printObj p@PInt{}  = return $ shows p
printObj p@PBool{} = return $ shows p
printObj p@PStr{}  = return $ shows p
printObj p@PChar{} = return $ shows p
printObj PUnit     = return $ shows PUnit
printObj e@Thunk{} = do
    o <- evalThunk e
    printObj o
printObj (Var (Just ptr)) = do
    o <- getsObjs (Map.! ptr)
    printObj o
printObj (Value t d) = do
    ss <- mapM showFld (Map.toList d)
    let s = foldr (.) id (intersperse (" " ++) ss)
    return $ shows t . (" { " ++) . s . (" }" ++)
  where
    showFld (i, ptr) = do
        o <- getsObjs (Map.! ptr)
        s <- printObj o
        return $ showsPrt i . (": " ++) . s
printObj Fun{} = return ("<fun>" ++)


-- Native functions.

nativeObjs :: Interpreter OEnv
nativeObjs = do
    let (is, objs) = unzip decls
        n          = length is
    ls <- newlocs n
    let lsobjs = zip ls objs
    modifyObjs (Map.union $ Map.fromList lsobjs)
    return $ Map.fromList (zip is ls)
  where
    p1 = Ident "a"
    se = Ident "()"
    decls =
        [ (Ident "print"  , Fun [p1, se] printBody Map.empty)
        , (Ident "printLn", Fun [p1, se] printLnBody Map.empty)
        ]
    printBody = do
        o <- getObj p1
        s <- printObj o
        raise $ output s
        return PUnit
    printLnBody = do
        o <- getObj p1
        s <- printObj o
        raise $ output (s . ("\n" ++))
        return PUnit
    getObj i = do
        l <- asksObjs (Map.! i)
        getsObjs (Map.! l)
