{-# LANGUAGE FlexibleContexts #-}
module Harper.Interpreter.Alloc where
import           Control.Monad.State
import qualified Data.Map                     as Map

import           Harper.Interpreter.Core

alloc :: Object -> Interpreter Int
alloc o = do
    l <- newloc
    modifyObjs (Map.insert l o)
    return l

allocs :: Traversable t => t Object -> Interpreter (t Int)
allocs os = forM os alloc

newlocs :: Int -> Interpreter [Int]
newlocs n = do
    lookup <- getsObjs Map.lookupMax
    case lookup of
        Just (l, _) -> return $ genLocsFrom (l + 1)
        Nothing     -> return $ genLocsFrom 0
    where genLocsFrom l = [l .. l + n - 1]

newloc :: Interpreter Int
newloc = do
    l <- newlocs 1
    return $ head l
