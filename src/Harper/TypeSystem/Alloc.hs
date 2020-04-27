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
    case lookup of
        Just (l, _) -> return $ genLocsFrom (l + 1)
        Nothing     -> return $ genLocsFrom 0
    where genLocsFrom l = [l .. l + n - 1]

newloc :: TypeChecker Int
newloc = do
    l <- newlocs 1
    return $ head l
