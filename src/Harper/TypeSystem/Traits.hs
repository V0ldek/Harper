module Harper.TypeSystem.Traits where

import           Harper.TypeSystem.Core
import           Harper.TypeSystem.GlobalTypes

equatable :: Type -> Type -> Bool
equatable t1 t2 = not (funType t1) && not (funType t2) && t1 == t2

comparable :: Type -> Type -> Bool
comparable t1 t2 = primType t1 && primType t2 && t1 == t2

funType :: Type -> Bool
funType (FType _ _) = True
funType _           = False

primType :: Type -> Bool
primType (PType _) = True
primType _         = False

predicate :: Type -> Bool
predicate = (==) boolT
