module Harper.TypeSystem.Core where
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           Harper.Abs
import           Harper.Output
import           OutputM

type TEnv = Map.Map Ident Type
type CEnv = Map.Map UIdent TypeCtor
type OEnv = Map.Map Ident Type

data Env = Env { objs :: OEnv,
                 types :: TEnv,
                 tctors :: CEnv } deriving (Show, Eq)

data Type = VType { name :: UIdent, params :: [Ident], args :: [Type], ctors :: Set.Set UIdent }
          | FType { param :: Type, ret :: Type }
          | TypeVar Ident
          | PType UIdent
          deriving (Eq, Ord)

data TypeCtor = TypeCtor { tname :: UIdent, ctor :: UIdent, flds :: OEnv } deriving (Eq, Ord)

type TypeChecker a = ReaderT Env (Output ShowS) a

instance Show Type where
    showsPrec p (VType i _ []    _) = showsPrt i
    showsPrec p (VType i _ targs _) = showParen
        (p > 10)
        (showsPrt i . (" " ++) . foldr (.) id (intersperse (" "++) (map (showsPrec 11) targs)))
    showsPrec p (FType pt rt) =
        showParen (p > 10) (showsPrec 11 pt . (" -> " ++) . showsPrec 11 rt)
    showsPrec p (TypeVar i) = showsPrt i
    showsPrec p (PType   i) = showsPrt i

instance Show TypeCtor where
    showsPrec p (TypeCtor i c _) = showsPrt i . ("." ++) . showsPrt c

bindParam :: (Functor f) => Type -> Type -> f Type -> f Type
bindParam param binding = fmap (\p -> if param == p then binding else param)

localTypes :: (TEnv -> TEnv) -> TypeChecker a -> TypeChecker a
localTypes f = local (\env -> env { types = f $ types env })

localObjs :: (OEnv -> OEnv) -> TypeChecker a -> TypeChecker a
localObjs f = local (\env -> env { objs = f $ objs env })

raise :: HarperOutput a -> TypeChecker a
raise = lift
