module Harper.TypeSystem.Interfaces
    ( implementInterfaces
    )
where
import           Control.Monad.State
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           Harper.Abs
import           Harper.TypeSystem.Core
import           Harper.TypeSystem.GlobalTypes

implementInterfaces :: Type -> TypeChecker ()
implementInterfaces t@VType { vName = i, vMembs = membs, vUnivMembs = univ } =
    do
        ifaces <- getIfaces (Map.restrictKeys membs univ)
        let t' = t { vIfaces = Map.fromList (map (\i -> (iName i, i)) ifaces) }
        modify (\st -> st { types = Map.insert i t' $ types st })
implementInterfaces t@RType { rName = i, rMembs = membs } = do
    ifaces <- getIfaces membs
    let t' = t { rIfaces = Map.fromList (map (\i -> (iName i, i)) ifaces) }
    modify (\st -> st { types = Map.insert i t' $ types st })
implementInterfaces t = return ()

getIfaces :: OEnv -> TypeChecker [IfaceImpl]
getIfaces membs = do
    iterable    <- implIterable
    refIterable <- implRefIterable
    return $ iterable ++ refIterable
  where
    implIterable = case Map.lookup (Ident "iterate") membs of
        Just ptr -> do
            memb <- getByPtr ptr
            case memb of
                Obj (FType _ VType { vName = i, vArgs = args }) _
                    | i == iteratorI -> return [Iface iterableI args]
                _ -> return []
        Nothing -> return []
    implRefIterable = case Map.lookup (Ident "iterate") membs of
        Just ptr -> do
            memb <- getByPtr ptr
            case memb of
                Obj (FType _ RType { rName = i, rArgs = args }) _
                    | i == refIteratorI -> return [Iface refIterableI args]
                _ -> return []
        Nothing -> return []
