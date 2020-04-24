module Harper.Interpreter.Core where
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           Harper.Abs
import           Harper.Output
import           Harper.TypeSystem.Core         ( TypeCtor(..) )
import           OutputM

type Ptr = Int

type OEnv = Map.Map Ident Ptr
type Store = Map.Map Ptr Object
type TEnv = Map.Map UIdent TypeCtor
data Env = Env { objs :: OEnv,
                 types :: TEnv } deriving (Show, Eq)

type Interpreter a = ReaderT Env (StateT Store (Output ShowS)) a

data Object = Fun { params :: [Ident],
                    body :: Statement Pos,
                    env :: OEnv }
            | Thunk { expr :: Expression Pos,
                      env :: OEnv,
                      this :: Maybe Ptr }
            | Value { _type :: TypeCtor, _data :: OEnv }
            | Var   { ptr :: Maybe Ptr }
            | PInt Integer
            | PBool Bool
            | PStr String
            | PChar Char
            | PUnit
            deriving Eq

instance Show Object where
  show (PInt  n    ) = show n
  show (PBool True ) = "true"
  show (PBool False) = "false"
  show (PStr  s    ) = drop 1 $ init s
  show (PChar c    ) = show c
  show PUnit         = "()"
  show (Thunk e env ptr) =
    "Thunk " ++ show e ++ " " ++ show env ++ " " ++ show ptr
  show (Var o      ) = "Var " ++ show o
  show (Value t d  ) = show t ++ " " ++ showData d
  show (Fun p b env) = "Fun " ++ show p ++ " " ++ show b ++ " " ++ show env

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

-- Unspeakable name specifically chosen to be greater than all speakable names.
varKey :: String
varKey = "~var"

newvars :: Int -> OEnv -> [Ident]
newvars n env = case Map.lookupMax env of
  Nothing           -> genVarsFrom 1
  Just (Ident k, _) -> case stripPrefix varKey k of
    Nothing -> genVarsFrom 1
    Just v  -> genVarsFrom (read v + 1)
  where genVarsFrom v = map (\v' -> Ident (varKey ++ show v')) [v .. v + n - 1]

newvar :: OEnv -> Ident
newvar env = let [var] = newvars 1 env in var

raise :: HarperOutput a -> Interpreter a
raise = lift . lift
