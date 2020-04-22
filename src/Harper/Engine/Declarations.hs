module Harper.Engine.Declarations
    ( topLvlDecls
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
import           Harper.Engine.Core
import           Harper.Engine.Thunk

topLvlDecls :: [TopLvlDecl Pos] -> Interpreter Env
topLvlDecls ds = do
    let fds = [ fd | TopLvlFDecl _ fd <- ds ]
    let tds = [ td | TopLvlTDecl _ td <- ds ]
    oenv <- funDecls fds
    tenv <- typeDecls tds
    return $ Env oenv tenv

funDecls :: [FunDecl Pos] -> Interpreter OEnv
funDecls ds = do
    st  <- get
    env <- asks objs
    let n     = length ds
        ls    = newlocs n st
        is    = [ i | FDecl _ i _ _ <- ds ]
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
    paramIs ps = [ i | FArg _ i <- ps ]

typeDecls :: [TypeDecl Pos] -> Interpreter TEnv
typeDecls ds =
    let ts   = concatMap declToType ds
        is   = [ i | VType _ i _ <- ts ]
        ists = zip is ts
    in  return $ Map.fromList ists
  where
    declToType (ValTDecl a sig@(TSig _ i _) body) =
        declToType (ValTUDecl a sig [TVarDecl a i body])
    declToType (ValTUDecl _ (TSig _ i _) vs) = map variantToType vs
      where
        variantToType (TVarDecl _ i' body) = case body of
            DataTBody _ flds _ ->
                let fis = [ f | TFldDecl _ (THint _ f _) <- flds ]
                in  VType i i' (Set.fromList fis)
            TBody{} -> VType i i' Set.empty

declLocal :: LocalObjDecl Pos -> Expression Pos -> Interpreter OEnv
declLocal decl e = do
    val <- makeThunk e
    st <- get
    let l = newloc st
    modify (Map.insert l val)
    declLocal' decl l
            
declLocal' :: LocalObjDecl Pos -> Ptr -> Interpreter OEnv
declLocal' decl l = do
    st  <- get
    env <- asks objs
    let l' = newloc st
    case decl of
        LocVarDecl _ (Decl _ i) -> do
            let var = Var (Just l)
            modify (Map.insert l var)
            return $ Map.insert i l env
        LocValDecl _ (Decl _ i) -> return $ Map.insert i l env

declLocalUnass :: LocalObjDecl Pos -> Interpreter OEnv
declLocalUnass (LocVarDecl _ (Decl _ i)) = do
    st  <- get
    env <- asks objs
    let l   = newloc st
        var = Var Nothing
    modify (Map.insert l var)
    return $ Map.insert i l env
