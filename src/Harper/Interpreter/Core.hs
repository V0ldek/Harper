module Harper.Interpreter.Core where
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           Harper.Abs
import           Harper.Abs.Pos
import           Harper.Output
import           Harper.TypeSystem.Core         ( TypeMetaData )
import           Harper.Abs.Typed
import           OutputM

type Ptr = Int

type OEnv = Map.Map Ident Ptr
type TEnv = Map.Map UIdent ValueCtor
type ObjStore = Map.Map Ptr Object
data Store = St { objStore  :: ObjStore, varSrc :: Int } deriving Show
data Env = Env { objs :: OEnv,
                 types :: TEnv } deriving Show

type Interpreter = ReaderT Env (StateT Store (Output ShowS))

type Eval = Expression Meta -> Interpreter Object
type Exec = Statement Meta -> Interpreter Object

type Meta = TypeMetaData Pos

meta :: Expression Meta -> Meta
meta e = (typ e, pos e)

data Object = Fun { params :: [Ident],
                    body :: Interpreter Object,
                    env :: OEnv }
            | Thunk (Interpreter Object)
            | Value { _type :: ValueCtor, _data :: OEnv }
            | Var   { ptr :: Maybe Ptr }
            | PInt Integer
            | PBool Bool
            | PStr String
            | PChar Char
            | PUnit

data ValueCtor = ValueCtor { typeName :: UIdent, ctorName :: UIdent, membs :: OEnv }

instance Show Object where
  show (PInt  n    ) = show n
  show (PBool True ) = "true"
  show (PBool False) = "false"
  show (PStr  s    ) = s
  show (PChar c    ) = show c
  show PUnit         = "()"
  show (Thunk x    ) = "Thunk"
  show (Var   o    ) = "Var " ++ show o
  show (Value t d  ) = show t ++ " " ++ showData d
  show (Fun p b env) = "Fun " ++ show p ++ " " ++ show env

instance Show ValueCtor where
  showsPrec p (ValueCtor i c _) = showsPrt i . ("." ++) . showsPrt c

showData :: OEnv -> String
showData d =
  let
    s =
      intercalate ","
        $ map (\(Ident k, v) -> show k ++ ": " ++ show v)
        $ Map.toList d
  in  "{" ++ s ++ "}"

localObjs :: (OEnv -> OEnv) -> Interpreter a -> Interpreter a
localObjs f = local (\env -> env { objs = f $ objs env })

asksTypes :: (TEnv -> a) -> Interpreter a
asksTypes f = asks (f . types)

asksObjs :: (OEnv -> a) -> Interpreter a
asksObjs f = asks (f . objs)

getsObjs :: (ObjStore -> a) -> Interpreter a
getsObjs f = gets (f . objStore)

modifyObjs :: (ObjStore -> ObjStore) -> Interpreter ()
modifyObjs f = modify (\st -> st { objStore = f $ objStore st })

lookupObj :: Ident -> Interpreter (Maybe Object)
lookupObj i = do
  lookup <- asksObjs (Map.lookup i)
  case lookup of
    Just ptr -> do
      obj <- getsObjs (Map.! ptr)
      return $ Just obj
    Nothing -> return Nothing

getObj :: Ident -> Interpreter Object
getObj i = do
  l <- asksObjs (Map.! i)
  getsObjs (Map.! l)

inCurrentScope :: Interpreter a -> Interpreter (Interpreter a)
inCurrentScope a = do
  env <- ask
  return $ local (const env) a

inCurrentScope2 :: (a -> Interpreter b) -> Interpreter (a -> Interpreter b)
inCurrentScope2 a = do
  env <- ask
  return (local (const env) . a)

inCurrentScope3
  :: (a -> b -> Interpreter c) -> Interpreter (a -> b -> Interpreter c)
inCurrentScope3 a = do
  env <- ask
  return ((local (const env) .) . a)

objType :: Object -> String
objType (PInt  _)      = "Integer"
objType (PBool _)      = "Bool"
objType (PStr  _)      = "String"
objType (PChar _)      = "Char"
objType PUnit          = "Unit"
objType Fun{}          = "Function"
objType (Value t _   ) = show t
objType (Var (Just o)) = "Variable"
objType _              = "undefined"

evalObj :: Object -> Interpreter (Maybe Object)
evalObj o = case o of
  Var (Just ptr) -> do
    o' <- getsObjs (Map.! ptr)
    evalObj o'
  Var   Nothing   -> return Nothing
  Thunk t         -> t >>= (return . Just)
  Fun [] body env -> localObjs (const env) body >>= (return . Just)
  _               -> return $ Just o

-- Unspeakable name specifically chosen to be greater than all speakable names.
varKey :: String
varKey = "~var"

newvars :: Int -> Interpreter [Ident]
newvars n = do
  v <- gets varSrc
  modify (\st -> st { varSrc = v + n })
  return $ map (\v' -> Ident (varKey ++ show v')) [v .. v + n - 1]

newvar :: Interpreter Ident
newvar = do
  var <- newvars 1
  return $ head var

raise :: HarperOutput a -> Interpreter a
raise = lift . lift
