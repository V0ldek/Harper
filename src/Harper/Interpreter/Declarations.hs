module Harper.Interpreter.Declarations
    ( typeDecls
    , funDecls
    , declLocal
    , declLocalUnass
    )
where
import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Control.Monad.Reader
import           Control.Monad.State

import           Harper.Abs
import           Harper.Interpreter.Alloc
import           Harper.Interpreter.Core
import           Harper.Interpreter.Iterator
import           Harper.Interpreter.Thunk
import           Harper.Output
import           Harper.TypeSystem.Core         ( ctorIdent
                                                , thisIdent
                                                )

typeDecls :: Call -> Exec -> [TypeDecl Meta] -> Interpreter TEnv
typeDecls call exec ds = do
    tenvs <- mapM (declCtors call exec) ds
    return $ foldl' Map.union Map.empty tenvs

declCtors :: Call -> Exec -> TypeDecl Meta -> Interpreter TEnv
declCtors call exec (ValTDecl a sig@(TSig _ tName _) body) =
    declCtors call exec (ValTUDecl a sig [TVarDecl a tName body])
declCtors call exec (ValTUDecl _ (TSig _ tName _) vs) = do
    cs <- mapM (declVCtor call exec tName) vs
    let iscs = zip (map (\(ValueCtor _ i _) -> i) cs) cs
    return $ Map.fromList iscs
declCtors call exec (RefTDecl _ (TSig _ tName _) body) = do
    c <- declRCtor call exec tName body
    return $ Map.fromList [(tName, c)]

declVCtor
    :: Call -> Exec -> UIdent -> TypeVariantDecl Meta -> Interpreter TCtor
declVCtor call exec tName (TVarDecl _ i body) = do
    let membs = case body of
            (TBody _ membs      ) -> membs
            (DataTBody _ _ membs) -> membs
        decls = [ decl | TMemFDecl _ decl <- membs ]
    autoMembs <- case body of
        TBody{}              -> return []
        (DataTBody _ flds _) -> generateAutoMembs flds
    ls <- allocs (map snd autoMembs)
    let autoEnv = Map.fromList (zip (map fst autoMembs) ls)
    declEnv <- funDecls decls call exec
    return $ ValueCtor tName i (Map.union declEnv autoEnv)
  where
    generateAutoMembs flds = do
        env <- asksObjs id
        return $ map
            (\(TFldDecl _ (THint _ i _)) -> (i, Fun [thisIdent] (getFld i) env))
            flds
    getFld i = do
        o   <- getObj thisIdent
        mo' <- evalObj o
        case mo' of
            Just (Inst _ _data) -> case Map.lookup i _data of
                Just l' -> do
                    o'   <- getsObjs (Map.! l')
                    mo'' <- evalObj o'
                    case mo'' of
                        Just o'' -> return o''
                        Nothing ->
                            error
                                $ "An object field is an unassigned variable. This should be impossible. "
                                ++ show i
                                ++ " "
                                ++ show mo''
                Nothing ->
                    error
                        $  "Invalid access to field "
                        ++ show i
                        ++ ". Type check should've caught this."
            _ ->
                error
                    $ "Invalid argument passed as `this` to a field getter. This should be impossible. "
                    ++ show i
                    ++ " "
                    ++ show mo'

declRCtor :: Call -> Exec -> UIdent -> TypeBody Meta -> Interpreter TCtor
declRCtor call exec tName body = do
    let (membs, flds) = case body of
            (TBody _ membs         ) -> (membs, [])
            (DataTBody _ flds membs) -> (membs, flds)
        decls = [ decl | TMemFDecl _ decl <- membs ]
    declEnv <- funDecls decls call exec
    let ctorPtr = declEnv Map.! ctorIdent
        t       = RefCtor tName declEnv
    () <- setupCtor ctorPtr flds t
    return t
  where
    setupCtor ptr flds t = do
        ctor <- getsObjs (Map.! ptr)
        let initData = Map.fromList
                [ (i, Var Nothing) | TFldDecl _ (THint _ i _) <- flds ]
            Fun ctorParams ctorBody ctorEnv = ctor
            ctor' = Fun ctorParams (transBody ctorBody) ctorEnv
        modifyObjs (Map.insert ptr ctor')
      where
        transBody body = do
            let dataIs     = [ i | TFldDecl _ (THint _ i _) <- flds ]
                dataLength = length dataIs
            dataPtrs <- allocs (replicate dataLength (Var Nothing))
            let initData = Map.fromList $ zip dataIs dataPtrs
                inst     = Inst t initData
            instPtr <- alloc inst
            thisPtr <- alloc (Ref instPtr)
            localObjs (Map.insert thisIdent thisPtr) body

funDecls :: [FunDecl Meta] -> Call -> Exec -> Interpreter OEnv
funDecls ds call exec = do
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
    declToFun env (FDecl _ _ params (FExprBody a body)) =
        case paramIs params of
            ps -> Fun ps (call $ RetExprStmt a body) env
    declToFun env (FDecl _ _ params (FStmtBody _ body)) =
        Fun (paramIs params) (call body) env
    declToFun env (FDecl _ _ params (FVIterBody _ body)) =
        Fun (paramIs params) (iteratorBody body exec) env
    declToFun env (FDecl _ _ params (FRIterBody _ body)) =
        Fun (paramIs params) (refIteratorBody body exec) env
    paramIs ps = [ i | FParam _ i <- ps ]

declLocal :: LocalObjDecl Meta -> Ptr -> Interpreter OEnv
declLocal decl l = do
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
