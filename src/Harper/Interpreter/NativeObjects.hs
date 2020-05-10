module Harper.Interpreter.NativeObjects where
import qualified Data.Map                      as Map

import           Harper.Abs
import           Harper.Interpreter.Alloc
import           Harper.Interpreter.Core
import           Harper.Interpreter.Print
import           Harper.Output

nativeObjs :: Interpreter OEnv
nativeObjs = do
    let (is, objs) = unzip decls
        n          = length is
    ls <- newlocs n
    let lsobjs = zip ls objs
    modifyObjs (Map.union $ Map.fromList lsobjs)
    return $ Map.fromList (zip is ls)
  where
    p1 = Ident "a"
    se = Ident "()"
    decls =
        [ (Ident "print"  , Fun [p1, se] printBody Map.empty)
        , (Ident "printLn", Fun [p1, se] printLnBody Map.empty)
        ]
    printBody = do
        o <- getObj p1
        s <- printObj o
        raise $ outputMsg s
        return PUnit
    printLnBody = do
        o <- getObj p1
        s <- printObj o
        raise $ outputMsg (s . ("\n" ++))
        return PUnit
