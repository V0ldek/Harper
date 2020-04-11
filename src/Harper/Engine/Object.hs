module Harper.Engine.Object (
  Ptr, Store, Env,
  Object (..),
  isValue,
  intValue, boolValue, strValue, charValue,
  newloc, newlocs
)
where
import qualified Data.Map as Map

import Harper.Abs

type Ptr = Int
type Store = Map.Map Ptr Object
type Env = Map.Map Ident Ptr

data Object = Fun { params :: [Ident],
                    body :: Value,
                    env :: Env }
              | Thunk { body :: Value,
                        env :: Env }
              | PInt Integer
              | PBool Bool
              | PStr String
              | PChar Char
            deriving (Show, Eq, Ord)

isValue :: Object -> Bool
isValue Fun {}   = False
isValue Thunk {} = False
isValue _        = True

intValue :: Object -> Integer
intValue (PInt n) = n
intValue _        = error "Expected integer value"

boolValue :: Object -> Bool
boolValue (PBool b) = b
boolValue _         = error "Expected boolean value"

strValue :: Object -> String
strValue (PStr s) = s
strValue _        = error "Expected string value"

charValue :: Object -> Char
charValue (PChar c) = c
charValue _         = error "Expected char value"

newloc :: Store -> Ptr
newloc s 
    | s == Map.empty = 0
    | otherwise      = fst (Map.findMax s) + 1

newlocs :: Int -> Store -> [Ptr]
newlocs n s = let l = newloc s 
              in  [l..l + n - 1]
