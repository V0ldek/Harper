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
    (BlkSt reachable1 inLoop1 rets1 yields1 imp1 se1 used1 unass1) <> (BlkSt reachable2 inLoop2 rets2 yields2 imp2 se2 used2 unass2)
        = BlkSt (reachable1 || reachable2)
                (inLoop1 || inLoop2)
                (rets1 ++ rets2)
                (yields1 ++ yields2)
                (imp1 || imp2)
                (se1 || se2)
                (Set.union used1 used2)
                (Set.union unass1 unass1)

instance Monoid BlockState where
    mempty = BlkSt False False [] [] False False Set.empty Set.empty

modifyBlkSt :: (BlockState -> BlockState) -> TypeChecker ()
modifyBlkSt f = modify (\st -> st { blkSt = f (blkSt st) })

getsBlkSt :: (BlockState -> a) -> TypeChecker a
getsBlkSt f = gets (f . blkSt)

getBlkSt :: TypeChecker BlockState
getBlkSt = getsBlkSt id

initialBlkSt :: BlockState
initialBlkSt = BlkSt True False [] [] False False Set.empty Set.empty

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

usingObj :: Ptr -> TypeChecker ()
usingObj l = modifyBlkSt (\st -> st { usedObjs = Set.insert l (usedObjs st) })

visibleObjs :: TypeChecker (Set.Set Ptr)
visibleObjs = asksObjs (Set.fromList . Map.elems)

unassignedObj :: Ptr -> TypeChecker ()
unassignedObj l =
    modifyBlkSt (\st -> st { unassObjs = Set.insert l (unassObjs st) })

assignObj :: Ptr -> TypeChecker ()
assignObj l =
    modifyBlkSt (\st -> st { unassObjs = Set.delete l (unassObjs st) })

isDefAssigned :: Ptr -> TypeChecker Bool
isDefAssigned l = getsBlkSt ((not . Set.member l) . unassObjs)

unreachable :: TypeChecker ()
unreachable =
    modifyBlkSt (\st -> st { reachable = False, unassObjs = Set.empty })

enterLoop :: TypeChecker ()
enterLoop = modifyBlkSt (\st -> st { inLoop = True })

impure :: TypeChecker ()
impure = modifyBlkSt (\st -> st { isImpure = True })

sideeffect :: TypeChecker ()
sideeffect = modifyBlkSt (\st -> st { hasSideeffects = True })

mayEnterOneOf :: [BlockState] -> TypeChecker ()
mayEnterOneOf bs = do
    let b = mconcat bs
    modifyBlkSt
        (\st -> (st <> b) { reachable = reachable st
                          , inLoop    = inLoop st
                          , unassObjs = unassObjs st
                          }
        )

mustEnterOneOf :: [BlockState] -> TypeChecker ()
mustEnterOneOf bs = do
    let b = mconcat bs
    modifyBlkSt (const b)
