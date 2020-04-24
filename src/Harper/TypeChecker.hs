module Harper.TypeChecker
    ( runTypeChecker
    )
where
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                      as Map

import           Harper.Abs
import           Harper.Output
import           Harper.TypeSystem.Core
import           Harper.TypeSystem.Declarations

runTypeChecker
    :: Program Pos -> HarperOutput (Program Pos, Map.Map UIdent TypeCtor)
runTypeChecker tree =
    runReaderT (typeCheck tree) (Env Map.empty Map.empty Map.empty)

typeCheck :: Program Pos -> TypeChecker (Program Pos, Map.Map UIdent TypeCtor)
typeCheck p@(Prog _ ds) = do
    let tds = [ td | TopLvlTDecl _ td <- ds ]
    (tenv, cenv) <- typeDecls tds
    return (p, cenv)
