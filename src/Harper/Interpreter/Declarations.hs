module Harper.Interpreter.Declarations
    ( funDecls
    , declLocal
    , declLocal'
    , declLocalUnass
    )
where
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Control.Monad.Reader
import           Control.Monad.State

import           Harper.Abs
import           Harper.Alloc
import           Harper.Interpreter.Core
import           Harper.Interpreter.Thunk

funDecls :: [FunDecl Pos] -> Interpreter OEnv
funDecls ds = do
    st  <- get
    env <- asks objs
    let n = length ds
    ls <- newlocs n
    let is    = [ i | FDecl _ i _ _ <- ds ]
        isls  = zip is ls
        env'  = Map.fromList isls
        fobjs = map (declToFun (Map.union env' env)) ds
        lsfs  = zip ls fobjs
        st'   = Map.fromList lsfs
    modify (Map.union st')
    return env'
  where
    declToFun env (FDecl _ i params (FExprBody a body)) =
        case paramIs params of
            [] -> Thunk body env Nothing
            ps -> Fun ps (RetExprStmt a body) env
    declToFun env (FDecl _ i params (FStmtBody _ body)) =
        Fun (paramIs params) body env
    paramIs ps = [ i | FParam _ i <- ps ]

declLocal :: LocalObjDecl Pos -> Expression Pos -> Interpreter OEnv
declLocal decl e = do
    val <- makeThunk e
    l   <- newloc
    modify (Map.insert l val)
    declLocal' decl l

declLocal' :: LocalObjDecl Pos -> Ptr -> Interpreter OEnv
declLocal' decl l = do
    env <- asks objs
    l'  <- newloc
    case decl of
        LocVarDecl _ (Decl _ i) -> do
            let var = Var (Just l)
            modify (Map.insert l var)
            return $ Map.insert i l env
        LocValDecl _ (Decl _ i) -> return $ Map.insert i l env

declLocalUnass :: LocalObjDecl Pos -> Interpreter OEnv
declLocalUnass (LocVarDecl _ (Decl _ i)) = do
    env <- asks objs
    l   <- newloc
    let var = Var Nothing
    modify (Map.insert l var)
    return $ Map.insert i l env
