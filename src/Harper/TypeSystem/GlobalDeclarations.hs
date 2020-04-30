module Harper.TypeSystem.GlobalDeclarations where
import qualified Data.Map                      as Map

import           Harper.Abs
import           Harper.TypeSystem.Alloc
import           Harper.TypeSystem.Core
import           Harper.TypeSystem.GlobalTypes

declareGlobals :: TypeChecker OEnv
declareGlobals = do
    let (is, objs) = unzip globalObjs
    ls <- allocs objs
    return $ Map.fromList (zip is ls)
  where
    a = TypeVar (Ident "a")
    globalObjs =
        [ (Ident "print"  , Obj (FType a (FType SEType unitT)) False)
        , (Ident "printLn", Obj (FType a (FType SEType unitT)) False)
        ]
