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
import           Harper.Interpreter.Alloc
import           Harper.Interpreter.Core
import           Harper.Interpreter.Thunk

funDecls :: [FunDecl Meta] -> (Statement Meta -> Interpreter Object) -> Interpreter OEnv
funDecls ds interp = do
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
    modifyObjs (Map.union st')
    return env'
  where
    declToFun env (FDecl _ i params (FExprBody a body)) =
        case paramIs params of
            ps -> Fun ps (interp $ RetExprStmt a body) env
    declToFun env (FDecl _ i params (FStmtBody _ body)) =
        Fun (paramIs params) (interp body) env
    paramIs ps = [ i | FParam _ i <- ps ]

declLocal :: LocalObjDecl Meta -> Expression Meta -> Eval -> Interpreter OEnv
declLocal decl e eval = do
    (l, _) <- makeThunk e eval
    declLocal' decl l

declLocal' :: LocalObjDecl Meta -> Ptr -> Interpreter OEnv
declLocal' decl l = do
    env <- asks objs
    case decl of
        LocVarDecl _ d -> do
            let var = Var (Just l)
                i   = declId d
            l' <- newloc
            modifyObjs (Map.insert l' var)
            return $ Map.insert i l' env
        LocValDecl _ d -> let i = declId d in return $ Map.insert i l env

declLocalUnass :: LocalObjDecl Meta -> Interpreter OEnv
declLocalUnass (LocVarDecl _ d) = do
    env <- asks objs
    l   <- newloc
    let var = Var Nothing
        i   = declId d
    modifyObjs (Map.insert l var)
    return $ Map.insert i l env
declLocalUnass d =
    error
        $ "Declarations without assignment for types other than LocVarDecl should be disallowed by the grammar. Decl: "
        ++ show d

declId :: Declaration a -> Ident
declId (Decl      _ i            ) = i
declId (DeclWHint _ (THint _ i _)) = i
