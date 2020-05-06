{-# LANGUAGE FlexibleContexts #-}
module Harper.TypeSystem.Alloc where
import           Control.Monad.State
import qualified Data.Map                      as Map

import           Harper.TypeSystem.Core

alloc :: ObjData -> TypeChecker Int
alloc o = do
    l <- newloc
    modifyObjData (Map.insert l o)
    return l

allocs :: Traversable t => t ObjData -> TypeChecker (t Int)
allocs os = forM os alloc

newlocs :: Int -> TypeChecker [Int]
newlocs n = do
    lookup <- gets (Map.lookupMax . objData)
    let ls = case lookup of
            Just (l, _) -> genLocsFrom (l + 1)
            Nothing     -> genLocsFrom 0
    modifyObjData (Map.union $ Map.fromList (zip ls (repeat undefined)))
    return ls
    where genLocsFrom l = [l .. l + n - 1]

newloc :: TypeChecker Int
newloc = do
    l <- newlocs 1
    return $ head l
