module Harper.Interpreter.Core where
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import qualified Data.Map                      as Map
import           Data.Maybe
import qualified Data.Set                      as Set

import           Harper.Abs
import           Harper.Abs.Pos
import           Harper.Output
import           Harper.TypeSystem.Core         ( TypeMetaData
                                                , thisIdent
                                                )
import           Harper.Abs.Typed
import           OutputM

type Ptr = Int

type OEnv = Map.Map Ident Ptr
type TEnv = Map.Map UIdent TCtor
type ObjStore = Map.Map Ptr Object
data Store = St { objStore  :: ObjStore, varSrc :: Int } deriving Show
data Env = Env { objs :: OEnv,
                 types :: TEnv,
                 loopCnt :: Maybe (Interpreter Object),
                 loopBrk :: Maybe (Interpreter Object) }

type Interpreter = ReaderT Env (StateT Store (Output ShowS))

type Eval = Expression Meta -> Interpreter Object
type Exec = Statement Meta -> Interpreter Object
type ContExec
  =  Statement Meta
  -> (Object -> Interpreter Object)
  -> Interpreter Object
  -> Interpreter Object

type Meta = TypeMetaData Pos

meta :: Expression Meta -> Meta
meta e = (typ e, pos e)

data Object = Fun { params :: [Ident],
                    body :: Interpreter Object,
                    env :: OEnv }
            | Thunk (Interpreter Object)
            | Inst { _type :: TCtor, _data :: OEnv }
            | Ref   { ref :: Ptr }
            | Var   { var :: Maybe Ptr }
            | Tup { tupElems :: [Object] }
            | PInt Integer
            | PBool Bool
            | PStr String
            | PChar Char
            | PUnit

data TCtor = ValueCtor { vTypeName :: UIdent, ctorName :: UIdent, vMembs :: OEnv }
           | RefCtor { rTypeName :: UIdent, rMembs :: OEnv }

instance Show Object where
  show (PInt  n    ) = show n
  show (PBool True ) = "true"
  show (PBool False) = "false"
  show (PStr  s    ) = s
  show (PChar c    ) = show c
  show PUnit         = "()"
  show (Thunk x    ) = "Thunk"
  show (Var   l    ) = "Var " ++ show l
  show (Inst t d   ) = show t ++ " " ++ showData d
  show (Ref l      ) = "Ref " ++ show l
  show (Fun p b env) = "Fun " ++ show p ++ " " ++ show env

instance Show TCtor where
  showsPrec p (ValueCtor i c _) = showsPrt i . ("." ++) . showsPrt c
  showsPrec p (RefCtor i _    ) = showsPrt i

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

localLoop
  :: Interpreter Object -> Interpreter Object -> Interpreter a -> Interpreter a
localLoop kBrk kCnt =
  local (\env -> env { loopBrk = Just kBrk, loopCnt = Just kCnt })

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

getByPtr :: Ptr -> Interpreter Object
getByPtr ptr = getsObjs (Map.! ptr)

getThis :: Interpreter Object
getThis = do
  o  <- getObj thisIdent
  o' <- evalObj o
  case fromJust o' of
    Ref ptr -> getByPtr ptr
    o       -> return o

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
objType (PInt  _)  = "Integer"
objType (PBool _)  = "Bool"
objType (PStr  _)  = "String"
objType (PChar _)  = "Char"
objType PUnit      = "Unit"
objType Fun{}      = "Function"
objType (Inst t _) = show t
objType (Var _   ) = "Variable"
objType (Ref _   ) = "Reference"
objType _          = "undefined"

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
