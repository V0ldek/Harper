module Harper.Engine.Object
where
import qualified Data.Map as Map
import qualified Harper.Engine.Error as Error

import Harper.Abs

type Ptr = Int
type Store = Map.Map Ptr Object
type Env = Map.Map Ident Ptr

data Object = Fun { params :: [Ident],
                    body :: Statement Pos,
                    env :: Env }
            | Thunk { value :: Value Pos,
                      env :: Env }
            | PInt Integer
            | PBool Bool
            | PStr String
            | PChar Char
            | PUnit
            deriving (Eq, Ord)

instance Show Object where
  show (PInt n)    = show n
  show (PBool b)   = show b
  show (PStr s)    = drop 1 $ init s
  show (PChar c)   = show c
  show PUnit       = "()"
  show (Thunk v e) = "Thunk " ++ show v ++ " " ++ show e
  show (Fun p b e) = "Fun " ++ show p ++ " " ++ show b ++ " " ++ show e

isValue :: Object -> Bool
isValue Fun {}   = False
isValue Thunk {} = False
isValue _        = True

intValue :: Object -> Maybe Integer
intValue (PInt n) = return n
intValue o        = Nothing

boolValue :: Object -> Maybe Bool
boolValue (PBool b) = return b
boolValue o         = Nothing

strValue :: Object -> Maybe String
strValue (PStr s) = return s
strValue o        = Nothing

charValue :: Object -> Maybe Char
charValue (PChar c) = return c
charValue o         = Nothing

objType :: Object -> String
objType (PInt _) = "Integer"
objType (PBool _) = "Bool"
objType (PStr _) = "String"
objType (PChar _) = "Char"
objType PUnit = "Unit"
objType Fun {} = "Function"
objType _ = "undefined"

newloc :: Store -> Ptr
newloc s 
    | Map.null s = 0
    | otherwise  = fst (Map.findMax s) + 1

newlocs :: Int -> Store -> [Ptr]
newlocs n s = let l = newloc s 
              in  [l..l + n - 1]
