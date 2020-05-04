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

type Ptr = Int
type OEnv = Map.Map Ident Ptr

type ObjStore = Map.Map Ptr ObjData
type CtorStore = Map.Map UIdent TypeCtor
type TypeStore = Map.Map UIdent Type

data Store = St { blkSt :: BlockState, varSrc :: Int, types :: TypeStore, tCtors :: CtorStore, objData :: ObjStore }
data BlockState = BlkSt { reachable :: Bool, rets :: [Type], hasSideeffects :: Bool  } deriving Show
data ObjData = Obj { objType :: Type, assignable :: Bool } deriving (Eq, Ord, Show)

newtype Env = Env { objs :: OEnv } deriving (Show, Eq)

data Type = VType { vName :: UIdent, vParams :: [Ident], vArgs :: [Type], ctors :: Set.Set UIdent, vMembs :: OEnv }
          | RType { rName :: UIdent, rParams :: [Ident], rArgs :: [Type], rMembs :: OEnv, rData :: OEnv }
          | FType { param :: Type, ret :: Type }
          | TupType { tupElems :: [Type] }
          | TypeVar Ident
          | TypeBound Ident
          | PType UIdent
          | ImpType
          | SEType
          deriving (Eq, Ord)

name :: Type -> UIdent
name t@VType{} = vName t
name t@RType{} = rName t

args :: Type -> [Type]
args t@VType{} = vArgs t
args t@RType{} = rArgs t
args t         = []

membs :: Type -> OEnv
membs t@VType{} = vMembs t
membs t@RType{} = rMembs t
membs t         = Map.empty

data TypeCtor = TypeCtor { tname :: UIdent, ctor :: UIdent, flds :: Map.Map Ident ObjData } deriving (Eq, Ord)

type TypeChecker = ReaderT Env (StateT Store (Output ShowS))

instance Show Type where
    showsPrec p (VType i _ []    _ _) = showsPrt i
    showsPrec p (VType i _ targs _ _) = showsPrt i . (" " ++) . foldr
        (.)
        id
        (intersperse (" " ++) (map (showsPrec 11) targs))
    showsPrec p (RType i _ targs _ _) = showsPrt i . (" " ++) . foldr
        (.)
        id
        (intersperse (" " ++) (map (showsPrec 11) targs))
    showsPrec p (FType pt rt) =
        showParen (p > 10) (showsPrec 11 pt . (" -> " ++) . showsPrec p rt)
    showsPrec p (TypeVar   i    ) = showsPrt i
    showsPrec p (TypeBound i    ) = showsPrt i . ("&" ++)
    showsPrec p (PType     i    ) = showsPrt i
    showsPrec p (TupType   elems) = showParen
        True
        (foldr (.) id (intersperse (", " ++) (map (showsPrec 1) elems)))
    showsPrec p SEType  = ("sideeffect" ++)
    showsPrec p ImpType = ("impure" ++)

instance Show TypeCtor where
    showsPrec p (TypeCtor i c _) = showsPrt i . ("." ++) . showsPrt c

liftObjs :: (OEnv -> OEnv) -> Env -> Env
liftObjs f env = env { objs = f $ objs env }

localObjs :: (OEnv -> OEnv) -> TypeChecker a -> TypeChecker a
localObjs f = local $ liftObjs f

modifyObjData :: (ObjStore -> ObjStore) -> TypeChecker ()
modifyObjData f = modify (\st -> st { objData = f $ objData st })

modifyVarSrc :: (Int -> Int) -> TypeChecker ()
modifyVarSrc f = modify (\st -> st { varSrc = f $ varSrc st })

getsCtors :: (CtorStore -> a) -> TypeChecker a
getsCtors f = gets (f . tCtors)

getsTypes :: (TypeStore -> a) -> TypeChecker a
getsTypes f = gets (f . types)

asksObjs :: (OEnv -> a) -> TypeChecker a
asksObjs f = asks (f . objs)

lookupObj :: Ident -> TypeChecker (Maybe ObjData)
lookupObj i = do
    l <- asksObjs (Map.lookup i)
    case l of
        Just ptr -> do
            obj <- gets ((Map.! ptr) . objData)
            return $ Just obj
        Nothing -> return Nothing

lookupType :: UIdent -> TypeChecker (Maybe Type)
lookupType i = getsTypes (Map.lookup i)

getType :: UIdent -> TypeChecker Type
getType i = getsTypes (Map.! i)

loadTypes :: TypeStore -> TypeChecker ()
loadTypes t = modify (\st -> st { types = Map.union t (types st) })

ctorIdent :: Ident
ctorIdent = Ident "ctor"

-- Unspeakable name
thisIdent :: Ident
thisIdent = Ident "~this"

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