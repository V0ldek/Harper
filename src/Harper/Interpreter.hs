module Harper.Interpreter
    ( runInterpreter
    )
where
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Maybe
import           Data.List

import           Harper.Abs
import           Harper.Abs.Pos
import           Harper.Abs.Tuple
import           Harper.Abs.Typed
import qualified Harper.Error                  as Error
import           Harper.Expressions
import           Harper.Interpreter.Alloc
import           Harper.Interpreter.Conditionals
import           Harper.Interpreter.Core
import           Harper.Interpreter.Declarations
import           Harper.Interpreter.Iterable
import           Harper.Interpreter.Iterator
import           Harper.Interpreter.NativeObjects
import           Harper.Interpreter.PatternMatching
import           Harper.Interpreter.Print
import           Harper.Interpreter.Thunk
import           Harper.Output
import           Harper.Printer
import           Harper.TypeSystem.Core         ( TypeCtor(..)
                                                , ctorIdent
                                                , thisIdent
                                                )
import           Harper.TypeSystem.GlobalTypes
import           OutputM

runInterpreter :: Program Meta -> HarperOutput ShowS
runInterpreter tree = evalStateT
    (runReaderT (interpret tree) (Env Map.empty Map.empty Nothing Nothing))
    (St Map.empty 0)

interpret :: Program Meta -> Interpreter ShowS
interpret (Prog _ ds) = do
    let fds = [ fd | TopLvlFDecl _ fd <- ds ]
    let tds = [ td | TopLvlTDecl _ td <- ds ]
    globals <- nativeObjs
    userEnv <- localObjs (const globals) (funDecls fds call exec)
    let oenv = Map.union userEnv globals
    tenv <- localObjs (const oenv) (typeDecls call exec tds)
    let env = Env oenv tenv Nothing Nothing
    o <- local (const env) runMain
    printObj o

runMain :: Interpreter Object
runMain = do
    lookup <- lookupObj (Ident "main")
    case lookup of
        Just (Fun []           body env) -> localObjs (const env) body
        Just (Fun [Ident "()"] body env) -> localObjs (const env) body
        --Just (Thunk e env _) ->
        Just _                           -> raise Error.invMainType
        Nothing                          -> raise Error.undeclaredMain

eval :: Eval

-- Literals

eval (LitExpr _ (IntLit  _ n         )) = return $ PInt n
eval (LitExpr _ (BoolLit _ (BTrue  _))) = return $ PBool True
eval (LitExpr _ (BoolLit _ (BFalse _))) = return $ PBool False
eval (LitExpr _ (StrLit  _ s         )) = return $ PStr (unescape s)
  where
    -- BNFC automatically escapes control sequences, so "\n" in code becomes "\\n" in Haskell.
    -- But we want "\n" to be "\n" in Harper, so we unescape on string evaluation.
    unescape [] = []
    unescape s  = let [(esc, s')] = readLitChar s in esc : unescape s'
eval (  LitExpr _ (CharLit _ c)) = return $ PChar c
eval (  LitExpr _ (UnitLit _  )) = return PUnit

-- Value construction.

eval e@(VCtorExpr _ ctor flds  ) = do
    t <- getType ctor
    case t of
        ValueCtor{} -> do
            _data <- mapM fldAssToData flds
            return $ Inst t (Map.fromList _data)
  where
    fldAssToData (DataAss _ i e) = do
        (ptr, _) <- makeThunk e eval
        return (i, ptr)

-- Tuple.

eval (TupExpr _ tup) = do
    let es = tupToList tup
    os <- mapM eval es
    return $ Tup os

-- Object access.

eval e@(ObjExpr _ i) = do
    o   <- getObj i
    mo' <- evalObj o
    case mo' of
        Just o' -> return o'
        Nothing -> raise $ Error.unassVar i e

-- Function application.

eval (AppExpr _ e1 e2) = do
    o1 <- eval e1
    apply o1 e2

 -- Function sequencing.

eval (SeqExpr _ e1 e2) = do
    o1 <- eval e1
    apply o1 e2

-- Lambda expressions.

eval e@(LamExpr a params body) = do
    let n = length params
    env <- asks objs
    ps  <- newvars n
    let body' = case body of
            FExprBody  a e -> call $ RetExprStmt a e
            FStmtBody  _ s -> call s
            FVIterBody _ s -> iteratorBody s exec
            FRIterBody _ s -> refIteratorBody s exec
        pats = [ p | LamParam _ p <- params ]
    return $ Fun ps (matchAll (zip ps pats) body') env
  where
    matchAll ((p, pat) : ps) body = patMatchExpr
        eval
        pat
        (ObjExpr a p)
        (matchAll ps body)
        (raise $ Error.nonExhPatMatch e)
    matchAll [] body = body

-- Match expressions.

eval m@(MatchExpr _ e cs) = evalMatches e cs
  where
    evalMatches o (MatchExprClause _ p e : cs) =
        patMatchExpr eval p o (eval e) (evalMatches o cs)
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
eval e@(OrExpr _ e1 e2) = do
    o1 <- eval e1
    case o1 of
        PBool b1 -> if b1
            then return $ PBool True
            else do
                o2 <- eval e2
                case o2 of
                    PBool b2 -> return $ PBool b2
eval e'@(NotExpr _ e) = do
    o <- eval e
    case o of
        PBool b -> return $ PBool $ not b

-- Function composition.

eval e@(CompExpr a e1 e2) = do
    env <- asks objs
    var <- newvar
    let body = AppExpr a e1 (AppExpr a e2 (ObjExpr a var))
    return $ Fun [var] (eval body) env

-- Member access.

eval e@(MembExpr _ e' acc) = do
    o <- eval e'
    evalAcc o acc e

eval e@(TMembExpr _ _ []) =
    error
        "Type member access with an empty access list. Grammar should disallow this."
eval e@(TMembExpr _ tName (MembAcc _ i : acc)) | i == ctorIdent = do
    t <- getType tName
    case t of
        RefCtor _ membs -> do
            let ctorPtr = membs Map.! i
            ctor <- getByPtr ctorPtr
            evalAcc ctor acc e
eval e@(TMembExpr _ _ (a : acc)) = do
    env <- asks objs
    let o = Fun [thisIdent] accBody env in evalAcc o acc e
  where
    accBody = do
        o <- getObj thisIdent
        evalAcc o [a] e

-- This identifier.

eval (ThisExpr _) = getObj thisIdent

eval (DataExpr _ []) =
    error "Data access with an empty access list. Grammar should disallow this."
eval e@(DataExpr a (MembAcc _ i : acc)) = do
    inst <- getThis
    let fldPtr = _data inst Map.! i
    fld <- getByPtr fldPtr
    mo  <- evalObj fld
    case mo of
        Just o  -> evalAcc o acc e
        Nothing -> raise $ Error.unassVar i e

eval e =
    error ("Evaluating this type of values is not implemented yet: " ++ show e)

evalAcc
    :: (Print p, Position p)
    => Object
    -> [MemberAccess Meta]
    -> p
    -> Interpreter Object
evalAcc o []                      _   = return o
evalAcc o (e@(MembAcc _ i) : acc) ctx = do
    mo' <- evalObj o
    case mo' of
        Just (Inst (ValueCtor tName cName membs) _) ->
            case Map.lookup i membs of
                Just ptr -> continueAccess ptr
                Nothing  -> raise $ Error.notDeclOnVar tName cName i ctx
        Just (Ref ptr) -> do
            o <- getByPtr ptr
            let Inst (RefCtor tName membs) _ = o
            continueAccess (membs Map.! i)
  where
    continueAccess ptr = do
        memb <- getByPtr ptr
        mo'  <- evalObj memb
        case mo' of
            Just (Fun (p : ps) body env) -> do
                ptr <- alloc o
                let env' = Map.insert p ptr env
                o' <- case ps of
                    [] -> localObjs (const env') body
                    ps -> return $ Fun ps body env'
                evalAcc o' acc ctx
            Just o' ->
                error
                    $ "Member in member access is not a function type of arity > 0. Type check shoudl've caught this."
                    ++ show o'
            Nothing -> raise $ Error.unassVar i e

apply :: Object -> Eval
apply (Fun (p : ps) s env) argV = do
    (ptr, _) <- makeThunk argV eval
    let env' = Map.insert p ptr env
    case ps of
        [] -> localObjs (const env') s
        _  -> return $ Fun ps s env'
apply (Fun [] s env) (LitExpr _ (UnitLit _)) = localObjs (const env) s
apply o              _                       = return o

call :: Call
call s = exec s return f where f = return PUnit

-- Execution uses continuation-passing-style to implement control flow. 
-- Since statements can only be executed in a body of a function, they take at least two continuations:
-- the "return value" continuation and execution continuation. Calling the kRet short-circuits back to
-- the place of call. Using k continues the execution to the next statement.
exec :: Exec
exec (EmptyStmt _   ) _    k = k
exec (StmtBlock a ss) kRet k = do
    -- Scoping - make sure the continuations run in the scope from before entering the block.
    k'    <- inCurrentScope k
    kRet' <- inCurrentScope2 kRet
    execSeq ss kRet' k'
  where
    execSeq []       kRet k = k
    execSeq (s : ss) kRet k = exec s kRet (execSeq ss kRet k)

-- Control flow.

exec (RetExprStmt _ e) kRet _ = eval e >>= kRet
exec (RetStmt _      ) kRet _ = kRet PUnit
exec (YieldStmt _ e  ) kRet k = do
    o  <- eval e
    k' <- inCurrentScope k
    kRet (Tup [o, Thunk k'])
exec (  YieldRetStmt _) kRet _ = kRet PUnit
exec s@(BrkStmt      _) _    _ = do
    kBrk <- asks loopBrk
    fromMaybe
        (error
        $ "Brk encountered, but no brk continuation. Type check should've caught this."
        ++ show s
        )
        kBrk
exec s@(CntStmt _) _ _ = do
    kCnt <- asks loopCnt
    fromMaybe
        (error
        $ "Cnt encountered, but no cnt continuation. Type check should've caught this."
        ++ show s
        )
        kCnt
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
exec w@(WhileStmt _ pred s) kRet k = do
    o <- eval pred
    case o of
        PBool True -> do
            k'    <- inCurrentScope k
            kRet' <- inCurrentScope2 kRet
            let rep = exec w kRet' k'
            localLoop k' rep (exec s kRet' rep)
        PBool False -> k
exec f@ForInVStmt{} kRet k = do
    body <- forInIterable f
    exec body kRet k
exec f@ForInRStmt{} kRet k = do
    body <- forInRefIterable f
    exec body kRet k

exec f@ForInStmt{} _ _ =
    error
        $ "ForInStmt in interpreter, should've been converted during type check."
        ++ show f
exec m@(MatchStmt _ e cs) kRet k = execMatches e cs
  where
    execMatches e (MatchStmtClause _ p s : cs) =
        patMatchExpr eval p e (exec s kRet k) (execMatches e cs)
    execMatches _ [] = raise $ Error.nonExhPatMatch m

-- Declarations.

exec (DeclStmt _ decl) _ k = do
    oenv <- declLocalUnass decl
    localObjs (const oenv) k
exec d@(DconStmt _ pat e) _ k =
    patMatchExpr eval pat e k (raise $ Error.nonExhPatMatch d)

-- Assignment.

exec s@(AssStmt _ i e) kRet k = do
    ptr <- asksObjs (Map.! i)
    o   <- getByPtr ptr
    case o of
        Var (Just ptr) -> do
            _ <- emplaceThunk ptr e eval
            k
        Var Nothing -> do
            (tPtr, _) <- makeThunk e eval
            modifyObjs (Map.insert ptr (Var $ Just tPtr))
            k
        o ->
            error
                $  "Invalid assignment. Type check should've caught this."
                ++ show s
exec (AddStmt a i e) kRet k =
    exec (AssStmt a i (AddExpr a (ObjExpr a i) e)) kRet k
exec (SubStmt a i e) kRet k =
    exec (AssStmt a i (SubExpr a (ObjExpr a i) e)) kRet k
exec (MulStmt a i e) kRet k =
    exec (AssStmt a i (MulExpr a (ObjExpr a i) e)) kRet k
exec (DivStmt a i e) kRet k =
    exec (AssStmt a i (DivExpr a (ObjExpr a i) e)) kRet k
exec (PowStmt a i e) kRet k =
    exec (AssStmt a i (PowExpr a (ObjExpr a i) e)) kRet k
exec (CompStmt a i e) kRet k =
    exec (AssStmt a i (CompExpr a (ObjExpr a i) e)) kRet k

exec s@(DataAssStmt _ i e) kRet k = do
    inst <- getThis
    let fldPtr = _data inst Map.! i
    fld <- getByPtr fldPtr
    case fld of
        Var (Just ptr) -> do
            _ <- emplaceThunk ptr e eval
            k
        Var Nothing -> do
            (ptr, _) <- makeThunk e eval
            modifyObjs (Map.insert fldPtr (Var $ Just ptr))
            k
        o ->
            error
                $  "Invalid assignment. Type check should've caught this."
                ++ show s
exec (DataAddStmt a i e) kRet k =
    exec (DataAssStmt a i (AddExpr a (DataExpr a [MembAcc a i]) e)) kRet k
exec (DataSubStmt a i e) kRet k =
    exec (DataAssStmt a i (SubExpr a (DataExpr a [MembAcc a i]) e)) kRet k
exec (DataMulStmt a i e) kRet k =
    exec (DataAssStmt a i (MulExpr a (DataExpr a [MembAcc a i]) e)) kRet k
exec (DataDivStmt a i e) kRet k =
    exec (DataAssStmt a i (DivExpr a (DataExpr a [MembAcc a i]) e)) kRet k
exec (DataPowStmt a i e) kRet k =
    exec (DataAssStmt a i (PowExpr a (DataExpr a [MembAcc a i]) e)) kRet k
exec (DataCompStmt a i e) kRet k =
    exec (DataAssStmt a i (CompExpr a (DataExpr a [MembAcc a i]) e)) kRet k

exec (EvalStmt _ e) kRet k = do
    _ <- eval e
    k

exec s _ _ =
    error
        ("Executing this type of statements is not implemented yet: " ++ show s)

evalIntBinOp :: Eval
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

evalEqOp :: Eval
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

evalCmpOp :: Eval
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
