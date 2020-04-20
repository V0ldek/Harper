module Harper.Engine.Core where
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import qualified Data.Map                      as Map

import           Harper.Abs
import           OutputM

type Ptr = Int
type Store = Map.Map Ptr Object
type Env = Map.Map Ident Ptr

type Interpreter a = ReaderT Env (StateT Store (Output ShowS)) a

data Object = Fun { params :: [Ident],
                    body :: Statement Pos,
                    env :: Env }
            | Thunk { expr :: Expression Pos,
                      env :: Env }
            | Var   { obj :: Maybe Object }
            | PInt Integer
            | PBool Bool
            | PStr String
            | PChar Char
            | PUnit
            deriving (Eq, Ord)

instance Show Object where
  show (PInt  n)     = show n
  show (PBool b)     = show b
  show (PStr  s)     = drop 1 $ init s
  show (PChar c)     = show c
  show PUnit         = "()"
  show (Thunk e rho) = "Thunk " ++ show e ++ " " ++ show rho
  show (Var o      ) = "Var " ++ show o
  show (Fun p b rho) = "Fun " ++ show p ++ " " ++ show b ++ " " ++ show rho

isValue :: Object -> Bool
isValue Fun{}   = False
isValue Thunk{} = False
isValue Var{}   = False
isValue _       = True

objType :: Object -> String
objType (PInt  _)      = "Integer"
objType (PBool _)      = "Bool"
objType (PStr  _)      = "String"
objType (PChar _)      = "Char"
objType PUnit          = "Unit"
objType Fun{}          = "Function"
objType (Var (Just o)) = objType o
objType _              = "undefined"

newloc :: Store -> Ptr
newloc s | Map.null s = 0
         | otherwise  = fst (Map.findMax s) + 1

newlocs :: Int -> Store -> [Ptr]
newlocs n s = let l = newloc s in [l .. l + n - 1]

-- Unspeakable name specifically chosen to be greater than all speakable names.
varKey :: String
varKey = "~var"

newvar :: Env -> Ident
newvar env = case Map.lookupMax env of
  Nothing           -> Ident (varKey ++ "1")
  Just (Ident k, _) -> case stripPrefix varKey k of
    Nothing -> Ident (varKey ++ "1")
    Just n  -> Ident (varKey ++ show (read n + 1))
