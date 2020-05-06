module Harper.Interpreter.Iterator
    ( iteratorBody
    , refIteratorBody
    )
where
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           Harper.Abs
import           Harper.Abs.Pos
import           Harper.Abs.Typed
import           Harper.Interpreter.Alloc
import           Harper.Interpreter.Core
import           Harper.Interpreter.Thunk
import qualified Harper.Error                  as Error
import           Harper.Output
import           Harper.TypeSystem.Core         ( thisIdent )
import           Harper.TypeSystem.GlobalTypes

iteratorBody :: Statement Meta -> ContExec -> Interpreter Object
iteratorBody s exec = do
    ctor <- iteratorCtor (ValueCtor (UIdent "Iterator") (UIdent "Iterator"))
                         valueCurrentImpl
                         valueNextImpl
    contL      <- alloc (Thunk $ iteratorCont s exec)
    lookupThis <- lookupObj thisIdent
    case lookupThis of
        Just this -> do
            funThisL <- alloc this
            return $ Inst
                ctor
                (Map.fromList [(iterContI, contL), (iterFunThisI, funThisL)])
        Nothing -> return $ Inst ctor (Map.fromList [(iterContI, contL)])
refIteratorBody :: Statement Meta -> ContExec -> Interpreter Object
refIteratorBody s exec = do
    ctor <- iteratorCtor (RefCtor (UIdent "RefIterator"))
                         refCurrentImpl
                         refNextImpl
    contL      <- alloc (Thunk $ refIteratorCont s exec)
    contVarL   <- alloc (Var $ Just contL)
    elemVarL   <- alloc (Var Nothing)
    lookupThis <- lookupObj thisIdent
    ref        <- case lookupThis of
        Just this -> do
            funThisL <- alloc this
            alloc $ Inst
                ctor
                (Map.fromList
                    [ (iterContI   , contVarL)
                    , (iterElemI   , elemVarL)
                    , (iterFunThisI, funThisL)
                    ]
                )
        Nothing -> alloc $ Inst
            ctor
            (Map.fromList [(iterContI, contVarL), (iterElemI, elemVarL)])
    return $ Ref ref

iteratorCtor
    :: (OEnv -> TCtor)
    -> Interpreter Object
    -> Interpreter Object
    -> Interpreter TCtor
iteratorCtor ctorCtor currentImpl nextImpl = do
    oenv <- asks objs
    ls   <- newlocs 3
    let isls = zip [iterCurrentI, iterNextI, iterateI] ls
    let t     = ctorCtor (Map.fromList isls)
        membs = [current oenv, next oenv, iterate oenv]
    modifyObjs (Map.union (Map.fromList $ zip ls membs))
    return t
  where
    current = Fun [thisIdent] currentImpl
    next    = Fun [thisIdent] nextImpl
    iterate = Fun [thisIdent] iterateImpl

valueCurrentImpl :: Interpreter Object
valueCurrentImpl = do
    this <- getThis
    case Map.lookup iterElemI (_data this) of
        Just ptr -> getByPtr ptr
        Nothing  -> raise Error.iterCurrNoElem
valueNextImpl :: Interpreter Object
valueNextImpl = do
    this <- getThis
    let contPtr = _data this Map.! iterContI
    cont <- getByPtr contPtr
    let Thunk t = cont
    t
refCurrentImpl :: Interpreter Object
refCurrentImpl = do
    this <- getThis
    let fldPtr = _data this Map.! iterElemI
    elemVar <- getByPtr fldPtr
    case elemVar of
        Var (Just ptr) -> getByPtr ptr
        Var Nothing    -> raise Error.iterCurrNoElem
refNextImpl :: Interpreter Object
refNextImpl = do
    this <- getThis
    let contVarPtr = _data this Map.! iterContI
    contVar <- getByPtr contVarPtr
    let Var (Just contPtr) = contVar
    cont <- getByPtr contPtr
    let Thunk t = cont
    t
iterateImpl :: Interpreter Object
iterateImpl = getThis

iteratorCont :: Statement Meta -> ContExec -> Interpreter Object
iteratorCont = startIterator kElem
  where
    kElem :: Object -> Interpreter Object
    kElem PUnit = do
        this <- getThis
        return $ Tup [PBool False, this]
    kElem (Tup [o, k]) = do
        this <- getThis
        lo   <- alloc o
        lk   <- alloc k
        let
            newIter = this
                { _data =
                    Map.insert iterContI lk $ Map.insert iterElemI lo $ _data
                        this
                }
        return $ Tup [PBool True, newIter]

refIteratorCont :: Statement Meta -> ContExec -> Interpreter Object
refIteratorCont = startIterator kElem
  where
    kElem :: Object -> Interpreter Object
    kElem PUnit        = return $ PBool False
    kElem (Tup [o, k]) = do
        this <- getThis
        lo   <- alloc o
        lk   <- alloc k
        let elemFldPtr = _data this Map.! iterElemI
            contFldPtr = _data this Map.! iterContI
        modifyObjs (Map.insert elemFldPtr (Var $ Just lo))
        modifyObjs (Map.insert contFldPtr (Var $ Just lk))
        return $ PBool True

startIterator
    :: (Object -> Interpreter Object)
    -> Statement Meta
    -> ContExec
    -> Interpreter Object
startIterator kElem s exec = do
    kRet <- inCurrentScope2 kElem
    k    <- inCurrentScope $ kElem PUnit
    this <- getThis
    let funThisL = _data this Map.! iterFunThisI
    localObjs (Map.insert thisIdent funThisL) (exec s kRet k)

iterContI :: Ident
iterContI = Ident "~cont"

iterElemI :: Ident
iterElemI = Ident "~elem"

iterFunThisI :: Ident
iterFunThisI = Ident "~funThis"
