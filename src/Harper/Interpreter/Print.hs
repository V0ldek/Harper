module Harper.Interpreter.Print where
import           Data.List
import qualified Data.Map                      as Map

import           Harper.Interpreter.Core
import           Harper.Output

printObj :: Object -> Interpreter ShowS
printObj p@PInt{}  = return $ shows p
printObj p@PBool{} = return $ shows p
printObj p@PStr{}  = return $ shows p
printObj p@PChar{} = return $ shows p
printObj PUnit     = return $ shows PUnit
printObj (Tup os)  = do
    ss <- mapM printObj os
    let s = foldr (.) id (intersperse (", " ++) ss)
    return $ showParen True s
printObj (Thunk t) = do
    o <- t
    printObj o
printObj (Var (Just ptr)) = do
    o <- getsObjs (Map.! ptr)
    printObj o
printObj (Inst t d) = do
    ss <- mapM showFld (Map.toList d)
    let s = foldr (.) id (intersperse (" " ++) ss)
    return $ shows t . (" { " ++) . s . (" }" ++)
  where
    showFld (i, ptr) = do
        o <- getsObjs (Map.! ptr)
        s <- printObj o
        return $ showsPrt i . (": " ++) . s
printObj Fun{} = return ("<fun>" ++)
