module Harper.Interpreter.Thunk where
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map

import Harper.Abs
import Harper.Alloc
import Harper.Interpreter.Core
import Harper.Interpreter.Snapshots

makeThunk :: Expression Pos -> Interpreter Object
makeThunk e = do
    env <- asks objs
    (e', env') <- snapshotExpr e
    return (Thunk e' (env' env) Nothing)

emplaceThunk :: Expression Pos -> Interpreter Object
emplaceThunk e = do
    env <- asks objs
    (e', env') <- snapshotExpr e
    l <- newloc
    let thunk = Thunk e' (env' env) (Just l)
    modify (Map.insert l thunk)
    return thunk
