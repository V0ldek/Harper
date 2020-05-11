module Harper.Interpreter.Thunk
    ( makeThunk
    , emplaceThunk
    )
where
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                      as Map

import           Harper.Abs
import           Harper.Abs.Tuple
import           Harper.Abs.Typed
import           Harper.Interpreter.Alloc
import           Harper.Interpreter.Core
import           Harper.Output
import           Harper.TypeSystem.Core         ( Type(..) )
import           Harper.TypeSystem.Typing

makeThunk :: Expression Meta -> Eval -> Interpreter (Ptr, Object)
makeThunk e eval = do
    -- Alloc undefined to make sure we don't lose the location to something else during thunkPrep.
    l <- alloc undefined
    t <- emplaceThunk l e eval
    return (l, t)

emplaceThunk :: Ptr -> Expression Meta -> Eval -> Interpreter Object
emplaceThunk l e eval = do
    t  <- thunkPrep e eval
    t' <- inCurrentScope $ runThunk t l
    modifyObjs (Map.insert l (Thunk t'))
    return $ Thunk t'
  where
    runThunk :: Interpreter Object -> Ptr -> Interpreter Object
    runThunk t l = do
        o <- t
        modifyObjs (Map.insert l o)
        return o

thunkPrep :: Expression Meta -> Eval -> Interpreter (Interpreter Object)
thunkPrep e@LitExpr{}               eval = return $ eval e

-- Value construction.

thunkPrep e@(VCtorExpr a ctor flds) eval = do
    let n = length flds
    vars     <- newvars n
    fwdDecls <- fwdDeclFlds (zip vars flds)
    let (flds', ls) = unzip fwdDecls
        varEnv      = Map.fromList $ zip vars ls
    return $ localObjs (Map.union varEnv) (eval (VCtorExpr a ctor flds'))
  where
    fwdDeclFlds = mapM fwdDecl
    fwdDecl (var, DataAss a i e) = do
        (ptr, _) <- makeThunk e eval
        return (DataAss a i (ObjExpr a var), ptr)

-- Tuples.

thunkPrep e@(TupExpr a tup) eval = do
    let es = tupToList tup
    eThunks <- mapM (`makeThunk` eval) es
    let ls = map fst eThunks
        n  = length ls
    vars <- newvars n
    let vsls = zip vars ls
        vses = zip vars es
        es'  = map (\(v, e) -> ObjExpr (meta e) v) vses
        tup' = tupFromList meta es'
    return $ localObjs (Map.union $ Map.fromList vsls) (eval $ TupExpr a tup')

-- Object access.

thunkPrep e@(ObjExpr a i) eval = do
    o <- getObj i
    case o of
        (Var (Just ptr)) -> do
            ptr' <- copy ptr
            i'   <- newvar
            return $ localObjs (Map.insert i' ptr') (eval $ ObjExpr a i')
        _ -> return $ eval e

-- Function application.

thunkPrep (AppExpr a e1 e2) eval = thunkPrepApp e1 e2 (AppExpr a) eval

-- Function sequencing.

thunkPrep (SeqExpr a e1 e2) eval = thunkPrepApp e1 e2 (SeqExpr a) eval

-- Lambda expressions.

thunkPrep e@LamExpr{}       eval = return $ eval e

-- Integer operators.

thunkPrep (AddExpr a e1 e2) eval = thunkPrepBin e1 e2 (AddExpr a) eval
thunkPrep (SubExpr a e1 e2) eval = thunkPrepBin e1 e2 (SubExpr a) eval
thunkPrep (MulExpr a e1 e2) eval = thunkPrepBin e1 e2 (MulExpr a) eval
thunkPrep (DivExpr a e1 e2) eval = thunkPrepBin e1 e2 (DivExpr a) eval
thunkPrep (PowExpr a e1 e2) eval = thunkPrepBin e1 e2 (PowExpr a) eval
thunkPrep (ModExpr a e1 e2) eval = thunkPrepBin e1 e2 (ModExpr a) eval

thunkPrep (NegExpr a e    ) eval = thunkPrepUn e (NegExpr a) eval

-- Equations.

thunkPrep (EqExpr  a e1 e2) eval = thunkPrepBin e1 e2 (EqExpr a) eval
thunkPrep (NEqExpr a e1 e2) eval = thunkPrepBin e1 e2 (NEqExpr a) eval

-- Comparisons.

thunkPrep (LEqExpr a e1 e2) eval = thunkPrepBin e1 e2 (LEqExpr a) eval
thunkPrep (GEqExpr a e1 e2) eval = thunkPrepBin e1 e2 (GEqExpr a) eval
thunkPrep (LtExpr  a e1 e2) eval = thunkPrepBin e1 e2 (LtExpr a) eval
thunkPrep (GtExpr  a e1 e2) eval = thunkPrepBin e1 e2 (GtExpr a) eval

-- Boolean operators.

thunkPrep (AndExpr a e1 e2) eval = thunkPrepBin e1 e2 (AndExpr a) eval
thunkPrep (OrExpr  a e1 e2) eval = thunkPrepBin e1 e2 (OrExpr a) eval

thunkPrep (NotExpr a e    ) eval = thunkPrepUn e (NotExpr a) eval

-- Function composition.

thunkPrep (CompExpr a e1 e2) eval = thunkPrepBin e1 e2 (CompExpr a) eval

-- Member access.

thunkPrep (MembExpr a e acc) eval =
    thunkPrepUn e (\e' -> MembExpr a e' acc) eval
thunkPrep e@TMembExpr{}                       eval = return $ eval e

-- This identifier.

thunkPrep e@ThisExpr{}                        eval = return $ eval e

thunkPrep e@(DataExpr a (MembAcc a' i : acc)) eval = do
    inst <- getThis
    let flds   = _data inst
        fldPtr = flds Map.! i
    fld <- getByPtr fldPtr
    case fld of
        Var (Just ptr) -> do
            ptr' <- copy ptr
            i'   <- newvar
            return $ localObjs (Map.insert i' ptr')
                               (eval $ MembExpr a (ObjExpr a' i') acc)
        _ -> return $ eval e

thunkPrep e _ = error
    ("Evaluating this type of expressions is not implemented yet: " ++ show e)

thunkPrepUn
    :: Expression Meta
    -> (Expression Meta -> Expression Meta)
    -> Eval
    -> Interpreter (Interpreter Object)
thunkPrepUn e f eval = do
    (l, _) <- makeThunk e eval
    var    <- newvar
    let objExpr = ObjExpr (meta e) var
    return $ localObjs (Map.insert var l) (eval (f objExpr))

thunkPrepBin
    :: Expression Meta
    -> Expression Meta
    -> (Expression Meta -> Expression Meta -> Expression Meta)
    -> Eval
    -> Interpreter (Interpreter Object)
thunkPrepBin e1 e2 f eval = do
    (l1, _) <- makeThunk e1 eval
    (l2, _) <- makeThunk e2 eval
    var1    <- newvar
    var2    <- newvar
    let objExpr1 = ObjExpr (meta e1) var1
        objExpr2 = ObjExpr (meta e2) var2
    return $ localObjs (Map.insert var1 l1 . Map.insert var2 l2)
                       (eval (f objExpr1 objExpr2))

thunkPrepApp
    :: Expression Meta
    -> Expression Meta
    -> (Expression Meta -> Expression Meta -> Expression Meta)
    -> Eval
    -> Interpreter (Interpreter Object)
thunkPrepApp e1 e2 f eval = case typ e1 of
    FType t _ | t == SEType || t == ImpType -> do
        o <- eval (f e1 e2)
        return $ return o
    _ -> thunkPrepBin e1 e2 f eval

copy :: Ptr -> Interpreter Ptr
copy l = do
    o <- getsObjs (Map.! l)
    alloc o
