module Harper.Engine.Alloc
where
import Control.Monad.State
import qualified Data.Map as Map

import Harper.Engine.Core

alloc :: Object -> Interpreter Ptr
alloc o = do
    l <- gets newloc
    modify (Map.insert l o)
    return l