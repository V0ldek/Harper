module Harper.Engine.Comparable (
  ComparableObject (..)
) where
import Harper.Abs
import Harper.Engine.Object

newtype ComparableObject = CmpObj Object

instance Eq ComparableObject where
    (CmpObj (PInt n1))  == (CmpObj (PInt n2))  = n1 == n2
    (CmpObj (PBool b1)) == (CmpObj (PBool b2)) = b1 == b2
    (CmpObj (PStr s1))  == (CmpObj (PStr s2))  = s1 == s2
    (CmpObj (PChar c1)) == (CmpObj (PChar c2)) = c1 == c2
    _ == _                                     = error "Type mismatch in equality operator"

instance Ord ComparableObject where
    compare (CmpObj (PInt n1)) (CmpObj (PInt n2))   = compare n1 n2
    compare (CmpObj (PStr s1)) (CmpObj (PStr s2))   = compare s1 s2
    compare (CmpObj (PChar c1)) (CmpObj (PChar c2)) = compare c1 c2
    compare _ _                                     = error "Type mismatch in comparison operator"