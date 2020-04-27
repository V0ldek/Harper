{-# LANGUAGE FlexibleInstances #-}
module Harper.TypeSystem.Core where
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           Harper.Abs
import           Harper.Abs.Pos
import           Harper.Output
import           OutputM

type TEnv = Map.Map Ident Type
type OEnv = Map.Map Ident Type
type Ctors = Map.Map UIdent TypeCtor
data Store = St { blkSt :: BlockState, varSrc :: Int, tCtors :: Ctors }
data BlockState = BlkSt { reachable :: Bool, rets :: [Type]  } deriving Show

data Env = Env { objs :: OEnv,
                 types :: TEnv } deriving (Show, Eq)

data Type = VType { name :: UIdent, params :: [Ident], args :: [Type], ctors :: Set.Set UIdent }
          | FType { param :: Type, ret :: Type }
          | TypeVar Ident
          | TypeBound Ident
          | PType UIdent
          deriving (Eq, Ord)

data TypeCtor = TypeCtor { tname :: UIdent, ctor :: UIdent, flds :: OEnv } deriving (Eq, Ord)

type TypeChecker = ReaderT Env (StateT Store (Output ShowS))

instance Show Type where
    showsPrec p (VType i _ []    _) = showsPrt i
    showsPrec p (VType i _ targs _) = showParen
        (p > 10)
        (showsPrt i . (" " ++) . foldr
            (.)
            id
            (intersperse (" " ++) (map (showsPrec 11) targs))
        )
    showsPrec p (FType pt rt) =
        showParen (p > 10) (showsPrec 11 pt . (" -> " ++) . showsPrec 11 rt)
    showsPrec p (TypeVar   i) = showsPrt i
    showsPrec p (TypeBound i) = showsPrt i . ("&" ++)
    showsPrec p (PType     i) = showsPrt i

instance Show TypeCtor where
    showsPrec p (TypeCtor i c _) = showsPrt i . ("." ++) . showsPrt c

liftObjs :: (OEnv -> OEnv) -> Env -> Env
liftObjs f env = env { objs = f $ objs env }

liftTypes :: (TEnv -> TEnv) -> Env -> Env
liftTypes f env = env { types = f $ types env }

localTypes :: (TEnv -> TEnv) -> TypeChecker a -> TypeChecker a
localTypes f = local $ liftTypes f

localObjs :: (OEnv -> OEnv) -> TypeChecker a -> TypeChecker a
localObjs f = local $ liftObjs f

modifyVarSrc :: (Int -> Int) -> TypeChecker ()
modifyVarSrc f = modify (\st -> st { varSrc = f $ varSrc st })

getsCtors :: (Ctors -> a) -> TypeChecker a
getsCtors f = gets (f . tCtors)

asksTypes :: (TEnv -> a) -> TypeChecker a
asksTypes f = asks (f . types)

asksObjs :: (OEnv -> a) -> TypeChecker a
asksObjs f = asks (f . objs)

-- Unspeakable name.
varKey :: String
varKey = "~tVar"

newvars :: Int -> TypeChecker [Ident]
newvars n = do
    v <- gets varSrc
    modifyVarSrc (+ n)
    return $ map (\v' -> Ident (varKey ++ show v')) [v .. v + n - 1]

newvar :: TypeChecker Ident
newvar = do
    var <- newvars 1
    return $ head var

raise :: HarperOutput a -> TypeChecker a
raise = lift . lift

type TypeMetaData a = (Type, a)

annWith :: Type -> Pos -> TypeMetaData Pos
annWith t p = (t, p)
