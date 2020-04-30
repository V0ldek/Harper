module Harper.TypeSystem.StaticAnalysis where
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Maybe
import           Data.Monoid
import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           Harper.Abs
import           Harper.TypeSystem.Core

instance Semigroup BlockState where
    (BlkSt reachable1 rets1 se1) <> (BlkSt reachable2 rets2 se2) =
        BlkSt (reachable1 || reachable2) (rets1 ++ rets2) (se1 || se2)

instance Monoid BlockState where
    mempty = BlkSt False [] False

modifyBlkSt :: (BlockState -> BlockState) -> TypeChecker ()
modifyBlkSt f = modify (\st -> st { blkSt = f (blkSt st) })

getsBlkSt :: (BlockState -> a) -> TypeChecker a
getsBlkSt f = gets (f . blkSt)

getBlkSt :: TypeChecker BlockState
getBlkSt = getsBlkSt id

clearBlkSt :: TypeChecker ()
clearBlkSt = modifyBlkSt (const (BlkSt True [] False))

blockScope :: TypeChecker a -> TypeChecker (a, BlockState)
blockScope a = do
    before <- getBlkSt
    res    <- a
    after  <- getBlkSt
    modifyBlkSt (const before)
    return (res, after)

addRet :: Type -> TypeChecker ()
addRet t = modifyBlkSt (\st -> st { rets = t : rets st })

unreachable :: TypeChecker ()
unreachable = modifyBlkSt (\st -> st { reachable = False })

sideeffect :: TypeChecker ()
sideeffect = modifyBlkSt (\st -> st { hasSideeffects = True })

mayEnterOneOf :: [BlockState] -> TypeChecker ()
mayEnterOneOf bs = do
    let b = mconcat bs
    forM_ (rets b) addRet
    when (hasSideeffects b) sideeffect

mustEnterOneOf :: [BlockState] -> TypeChecker ()
mustEnterOneOf bs = do
    let b = mconcat bs
    modifyBlkSt (const b)
