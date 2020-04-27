module Harper.TypeSystem.GlobalTypes where
import qualified Data.Map                      as Map

import           Harper.Abs
import           Harper.TypeSystem.Core

globalTypes :: TEnv
globalTypes = Map.fromList
    [ (Ident "Integer", integerT)
    , (Ident "Bool"   , boolT)
    , (Ident "Char"   , charT)
    , (Ident "String" , stringT)
    , (Ident "Unit"   , unitT)
    ]

integerT :: Type
integerT = PType (UIdent "Integer")

boolT :: Type
boolT = PType (UIdent "Bool")

charT :: Type
charT = PType (UIdent "Char")

stringT :: Type
stringT = PType (UIdent "String")

unitT :: Type
unitT = PType (UIdent "Unit")
