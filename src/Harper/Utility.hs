module Harper.Utility where
import           Control.Monad
import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import Harper.Abs

-- Equivalent of Map.fromList, but fails if the keys are not unique
-- and returns the conflict tuple - key, value1, value2
mapFromListUnique :: Ord k => [(k, a)] -> Either (k, a, a) (Map.Map k a)
mapFromListUnique = foldM addIfUnique Map.empty
  where
    addIfUnique m (k, a) = case Map.lookup k m of
        Just a' -> Left (k, a, a')
        Nothing -> Right (Map.insert k a m)

-- MapFromListUnique but with a selector function.
mapFromListUniqueBy :: Ord k => (a -> (k, v)) -> [a] -> Either (k, v, v) (Map.Map k v)
mapFromListUniqueBy f = foldM addIfUnique Map.empty
  where
    addIfUnique m a = let (k, v) = f a
                      in  case Map.lookup k m of
                              Just v' -> Left (k, v, v')
                              Nothing -> Right (Map.insert k v m)

-- Equivalent of Set.fromList, but fails if the keys are not unique
-- and returns the conflicting key
setFromListUnique :: Ord k => [k] -> Either k (Set.Set k)
setFromListUnique = setFromListUniqueBy id

-- SetFromListUnique but with a selector function.
setFromListUniqueBy :: Ord k => (a -> k) -> [a] -> Either a (Set.Set k)
setFromListUniqueBy f = foldM addIfUnique Set.empty
  where
    addIfUnique s a = let k = f a
                      in if Set.member k s then Left a else Right (Set.insert k s)

itu :: Ident -> UIdent
itu (Ident s) = UIdent s

uti :: UIdent -> Ident
uti (UIdent s) = Ident s

its :: Ident -> String
its (Ident s) = s

uts :: UIdent -> String
uts (UIdent s) = s
