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
    (BlkSt reachable1 rets1 yields1 imp1 se1) <> (BlkSt reachable2 rets2 yields2 imp2 se2)
        = BlkSt (reachable1 || reachable2)
                (rets1 ++ rets2)
                (yields1 ++ yields2)
                (imp1 || imp2)
                (se1 || se2)

instance Monoid BlockState where
    mempty = BlkSt False [] [] False False

modifyBlkSt :: (BlockState -> BlockState) -> TypeChecker ()
modifyBlkSt f = modify (\st -> st { blkSt = f (blkSt st) })

getsBlkSt :: (BlockState -> a) -> TypeChecker a
getsBlkSt f = gets (f . blkSt)

getBlkSt :: TypeChecker BlockState
getBlkSt = getsBlkSt id

initialBlkSt :: BlockState
initialBlkSt = BlkSt True [] [] False False

clearBlkSt :: TypeChecker ()
clearBlkSt = modifyBlkSt (const initialBlkSt)

blockScope :: TypeChecker a -> TypeChecker (a, BlockState)
blockScope a = do
    before <- getBlkSt
    res    <- a
    after  <- getBlkSt
    modifyBlkSt (const before)
    return (res, after)

addRet :: Type -> TypeChecker ()
addRet t = modifyBlkSt (\st -> st { rets = t : rets st })

addYield :: Type -> TypeChecker ()
addYield t = modifyBlkSt (\st -> st { yields = t : yields st })

unreachable :: TypeChecker ()
unreachable = modifyBlkSt (\st -> st { reachable = False })

impure :: TypeChecker ()
impure = modifyBlkSt (\st -> st { isImpure = True })

sideeffect :: TypeChecker ()
sideeffect = modifyBlkSt (\st -> st { hasSideeffects = True })

mayEnterOneOf :: [BlockState] -> TypeChecker ()
mayEnterOneOf bs = do
    let b = mconcat bs
    forM_ (rets b)   addRet
    forM_ (yields b) addYield
    when (isImpure b)       impure
    when (hasSideeffects b) sideeffect

mustEnterOneOf :: [BlockState] -> TypeChecker ()
mustEnterOneOf bs = do
    let b = mconcat bs
    modifyBlkSt (const b)
