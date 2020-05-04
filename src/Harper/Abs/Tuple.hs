module Harper.Abs.Tuple where
import           Harper.Abs
import           Harper.Abs.Pos

tTupToList :: TupleType a -> [TypeExpr a]
tTupToList (TTupTail _ tExpr1 tExpr2) = [tExpr1, tExpr2]
tTupToList (TTupList _ tExpr  tail  ) = tExpr : tTupToList tail

tupToList :: TupleExpression a -> [Expression a]
tupToList (TupExprTail _ e1 e2  ) = [e1, e2]
tupToList (TupExprList _ e  tail) = e : tupToList tail

tupFromList :: (Expression a -> a) -> [Expression a] -> TupleExpression a
tupFromList _ []       = error "tupFromList empty"
tupFromList _ [e]      = error "tupFromList singleton"
tupFromList f [e1, e2] = TupExprTail (f e1) e1 e2
tupFromList f (e : es) = TupExprList (f e) e $ tupFromList f es

patTupToList :: TuplePattern a -> [Pattern a]
patTupToList (PatTupTail _ pat1 pat2) = [pat1, pat2]
patTupToList (PatTupList _ pat1 tail) = pat1 : patTupToList tail

patTupFromList :: (Pattern a -> a) -> [Pattern a] -> TuplePattern a
patTupFromList _ []           = error "patTupFromList empty"
patTupFromList _ [pat]        = error "patTupFromList singleton"
patTupFromList f [pat1, pat2] = PatTupTail (f pat1) pat1 pat2
patTupFromList f (pat : pats) = PatTupList (f pat) pat $ patTupFromList f pats
