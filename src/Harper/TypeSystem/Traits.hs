module Harper.TypeSystem.Traits where
import qualified Data.Map                      as Map

import           Harper.TypeSystem.Core
import           Harper.TypeSystem.GlobalTypes
import           Harper.TypeSystem.Typing

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

iterable :: Type -> Type -> Bool
iterable t elemT = case t of
    VType { vIfaces = ifaces } -> implements ifaces
    RType { rIfaces = ifaces } -> implements ifaces
    _                          -> False
  where
    implements ifaces = case Map.lookup iterableI ifaces of
        Just Iface { iArgs = args } ->
            let [tArg] = args in canUnify elemT tArg
        Nothing -> False

refIterable :: Type -> Type -> Bool
refIterable t elemT = case t of
    VType { vIfaces = ifaces } -> implements ifaces
    RType { rIfaces = ifaces } -> implements ifaces
    _                          -> False
  where
    implements ifaces = case Map.lookup refIterableI ifaces of
        Just Iface { iArgs = args } ->
            let [tArg1, _] = args in canUnify elemT tArg1
        Nothing -> False
