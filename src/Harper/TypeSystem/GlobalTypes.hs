module Harper.TypeSystem.GlobalTypes where
import qualified Data.Map                      as Map

import           Harper.Abs
import           Harper.TypeSystem.Core

globalTypes :: TypeStore
globalTypes = Map.fromList
    [ (UIdent "Integer", integerT)
    , (UIdent "Bool"   , boolT)
    , (UIdent "Char"   , charT)
    , (UIdent "String" , stringT)
    , (UIdent "Unit"   , unitT)
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