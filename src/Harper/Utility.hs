module Harper.Utility where
import           Control.Monad
import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

import           Harper.Abs

findDups :: Ord a => [a] -> [a]
findDups ds = snd $ foldr checkForDup (Set.empty, []) ds
 where
  checkForDup a (s, dups) =
    if Set.member a s then (s, a : dups) else (Set.insert a s, dups)

findDupsBy :: Ord k => (a -> k) -> [a] -> ([k], [a])
findDupsBy f ds = collect $ foldr checkForDup (Map.empty, []) ds
 where
  checkForDup a (m, dups) =
    let k = f a
    in  if Map.member k m then (m, (k, a) : dups) else (Map.insert k a m, dups)
  collect (m, dups) =
    let (ks, as) = unzip dups in (ks, foldr (\k as' -> m Map.! k : as') as ks)
    
itu :: Ident -> UIdent
itu (Ident s) = UIdent s

uti :: UIdent -> Ident
uti (UIdent s) = Ident s

its :: Ident -> String
its (Ident s) = s

uts :: UIdent -> String
uts (UIdent s) = s
