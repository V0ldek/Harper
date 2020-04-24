{-# LANGUAGE FlexibleContexts #-}
module Harper.Alloc where
import           Control.Monad.State
import qualified Data.Map                      as Map

import           Harper.Interpreter.Core

alloc :: (MonadState (Map.Map Int a) m) => a -> m Int
alloc o = do
    l <- newloc
    modify (Map.insert l o)
    return l

allocs :: (Traversable t, MonadState (Map.Map Int a) m) => t a -> m (t Int)
allocs os = forM os alloc

newlocs :: (MonadState (Map.Map Int a) m) => Int -> m [Int]
newlocs n = do
    lookup <- gets Map.lookupMax
    case lookup of
        Just (l, _) -> return $ genLocsFrom (l + 1)
        Nothing     -> return $ genLocsFrom 0
    where genLocsFrom l = [l .. l + n - 1]

newloc :: (MonadState (Map.Map Int a) m) => m Int
newloc = do
    l <- newlocs 1
    return $ head l
