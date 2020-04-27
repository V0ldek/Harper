module Harper.TypeSystem.StaticAnalysis where
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Maybe
import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           Harper.Abs
import           Harper.TypeSystem.Core

scopeDefAss :: TypeChecker a -> TypeChecker (a, DAss)
scopeDefAss a = do
    before <- gets defAss
    res    <- a
    after  <- gets defAss
    modifyDefAss (const before)
    return (res, after)

scopeObjs :: [Ident] -> OEnv -> TypeChecker a -> TypeChecker a
scopeObjs is oenv a = do
    before <- gets defAss
    let shadow    = Set.fromList is
        assBefore = Set.intersection before shadow
    res   <- localObjs (Map.union oenv) a
    after <- gets defAss
    let actAfter = Set.union (after Set.\\ shadow) assBefore
    modifyDefAss (const actAfter)
    return res

addRetT :: Type -> TypeChecker ()
addRetT r = modifyRets (Just r :)

addEndBlk :: TypeChecker ()
addEndBlk = modifyRets (Nothing :)

consumeRets :: TypeChecker [Maybe Type]
consumeRets = do
    rets <- gets rets
    modifyRets (const [])
    return rets
