{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Harper.Parser where
import Harper.Abs
import Harper.Lexer
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: ((Maybe (Int, Int), Ident)) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Ident))
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: ((Maybe (Int, Int), Integer)) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Integer))
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: ((Maybe (Int, Int), Char)) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Char))
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: ((Maybe (Int, Int), String)) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), String))
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: ((Maybe (Int, Int), UIdent)) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), UIdent))
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ((Maybe (Int, Int), Program (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Program (Maybe (Int, Int))))
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ((Maybe (Int, Int), TopLvlDecl (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), TopLvlDecl (Maybe (Int, Int))))
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ((Maybe (Int, Int), [TopLvlDecl (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [TopLvlDecl (Maybe (Int, Int))]))
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: ((Maybe (Int, Int), TypeHint (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), TypeHint (Maybe (Int, Int))))
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: ((Maybe (Int, Int), TypeExpr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), TypeExpr (Maybe (Int, Int))))
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ((Maybe (Int, Int), TypeExpr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), TypeExpr (Maybe (Int, Int))))
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: ((Maybe (Int, Int), TypeExpr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), TypeExpr (Maybe (Int, Int))))
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: ((Maybe (Int, Int), [TypeExpr (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [TypeExpr (Maybe (Int, Int))]))
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: ((Maybe (Int, Int), TupleType (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), TupleType (Maybe (Int, Int))))
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: ((Maybe (Int, Int), TypePurity (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), TypePurity (Maybe (Int, Int))))
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ((Maybe (Int, Int), FieldTypeExpr (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), FieldTypeExpr (Maybe (Int, Int))))
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: ((Maybe (Int, Int), [FieldTypeExpr (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [FieldTypeExpr (Maybe (Int, Int))]))
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: ((Maybe (Int, Int), FunDecl (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), FunDecl (Maybe (Int, Int))))
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: ((Maybe (Int, Int), FunParam (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), FunParam (Maybe (Int, Int))))
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: ((Maybe (Int, Int), [FunParam (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [FunParam (Maybe (Int, Int))]))
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: ((Maybe (Int, Int), LambdaParam (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), LambdaParam (Maybe (Int, Int))))
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: ((Maybe (Int, Int), [LambdaParam (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [LambdaParam (Maybe (Int, Int))]))
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: ((Maybe (Int, Int), FunBody (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), FunBody (Maybe (Int, Int))))
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: ((Maybe (Int, Int), BoolLiteral (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), BoolLiteral (Maybe (Int, Int))))
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: ((Maybe (Int, Int), Literal (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Literal (Maybe (Int, Int))))
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: ((Maybe (Int, Int), MemberAccess (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), MemberAccess (Maybe (Int, Int))))
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: ((Maybe (Int, Int), [MemberAccess (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [MemberAccess (Maybe (Int, Int))]))
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: ((Maybe (Int, Int), Expression (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expression (Maybe (Int, Int))))
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: ((Maybe (Int, Int), Expression (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expression (Maybe (Int, Int))))
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: ((Maybe (Int, Int), Expression (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expression (Maybe (Int, Int))))
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: ((Maybe (Int, Int), Expression (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expression (Maybe (Int, Int))))
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: ((Maybe (Int, Int), Expression (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expression (Maybe (Int, Int))))
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: ((Maybe (Int, Int), Expression (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expression (Maybe (Int, Int))))
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: ((Maybe (Int, Int), Expression (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expression (Maybe (Int, Int))))
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: ((Maybe (Int, Int), Expression (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expression (Maybe (Int, Int))))
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: ((Maybe (Int, Int), Expression (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expression (Maybe (Int, Int))))
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: ((Maybe (Int, Int), Expression (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expression (Maybe (Int, Int))))
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: ((Maybe (Int, Int), Expression (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expression (Maybe (Int, Int))))
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: ((Maybe (Int, Int), Expression (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expression (Maybe (Int, Int))))
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: ((Maybe (Int, Int), Expression (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expression (Maybe (Int, Int))))
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: ((Maybe (Int, Int), Expression (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Expression (Maybe (Int, Int))))
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: ((Maybe (Int, Int), TupleExpression (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), TupleExpression (Maybe (Int, Int))))
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: ((Maybe (Int, Int), MatchExpressionClause (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), MatchExpressionClause (Maybe (Int, Int))))
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: ((Maybe (Int, Int), [MatchExpressionClause (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [MatchExpressionClause (Maybe (Int, Int))]))
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: ((Maybe (Int, Int), FieldAss (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), FieldAss (Maybe (Int, Int))))
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: ((Maybe (Int, Int), [FieldAss (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [FieldAss (Maybe (Int, Int))]))
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: ((Maybe (Int, Int), Statement (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Statement (Maybe (Int, Int))))
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: ((Maybe (Int, Int), Statement (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Statement (Maybe (Int, Int))))
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: ((Maybe (Int, Int), Statement (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Statement (Maybe (Int, Int))))
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: ((Maybe (Int, Int), Statement (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Statement (Maybe (Int, Int))))
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyIn54 :: ((Maybe (Int, Int), Statement (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Statement (Maybe (Int, Int))))
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyIn55 :: ((Maybe (Int, Int), Statement (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Statement (Maybe (Int, Int))))
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
happyIn56 :: ((Maybe (Int, Int), [Statement (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [Statement (Maybe (Int, Int))]))
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
happyIn57 :: ((Maybe (Int, Int), MatchStatementClause (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), MatchStatementClause (Maybe (Int, Int))))
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
happyIn58 :: ((Maybe (Int, Int), [MatchStatementClause (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [MatchStatementClause (Maybe (Int, Int))]))
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
happyIn59 :: ((Maybe (Int, Int), ConditionalStatement (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), ConditionalStatement (Maybe (Int, Int))))
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
happyIn60 :: ((Maybe (Int, Int), IfStatement (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), IfStatement (Maybe (Int, Int))))
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
happyIn61 :: ((Maybe (Int, Int), ElseIfStatement (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), ElseIfStatement (Maybe (Int, Int))))
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
happyIn62 :: ((Maybe (Int, Int), ElseStatement (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), ElseStatement (Maybe (Int, Int))))
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
happyIn63 :: ((Maybe (Int, Int), [ElseIfStatement (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [ElseIfStatement (Maybe (Int, Int))]))
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
happyIn64 :: ((Maybe (Int, Int), Pattern (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Pattern (Maybe (Int, Int))))
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
happyIn65 :: ((Maybe (Int, Int), Pattern (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Pattern (Maybe (Int, Int))))
happyOut65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut65 #-}
happyIn66 :: ((Maybe (Int, Int), Pattern (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Pattern (Maybe (Int, Int))))
happyOut66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut66 #-}
happyIn67 :: ((Maybe (Int, Int), TuplePattern (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), TuplePattern (Maybe (Int, Int))))
happyOut67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut67 #-}
happyIn68 :: ((Maybe (Int, Int), FieldPattern (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), FieldPattern (Maybe (Int, Int))))
happyOut68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut68 #-}
happyIn69 :: ((Maybe (Int, Int), [FieldPattern (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [FieldPattern (Maybe (Int, Int))]))
happyOut69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut69 #-}
happyIn70 :: ((Maybe (Int, Int), Declaration (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn70 #-}
happyOut70 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), Declaration (Maybe (Int, Int))))
happyOut70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut70 #-}
happyIn71 :: ((Maybe (Int, Int), AdHocFieldDecl (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn71 #-}
happyOut71 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), AdHocFieldDecl (Maybe (Int, Int))))
happyOut71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut71 #-}
happyIn72 :: ((Maybe (Int, Int), [AdHocFieldDecl (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn72 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn72 #-}
happyOut72 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [AdHocFieldDecl (Maybe (Int, Int))]))
happyOut72 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut72 #-}
happyIn73 :: ((Maybe (Int, Int), LocalFunDecl (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn73 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn73 #-}
happyOut73 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), LocalFunDecl (Maybe (Int, Int))))
happyOut73 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut73 #-}
happyIn74 :: ((Maybe (Int, Int), LocalObjDecl (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn74 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn74 #-}
happyOut74 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), LocalObjDecl (Maybe (Int, Int))))
happyOut74 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut74 #-}
happyIn75 :: ((Maybe (Int, Int), LocalObjDecl (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn75 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn75 #-}
happyOut75 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), LocalObjDecl (Maybe (Int, Int))))
happyOut75 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut75 #-}
happyIn76 :: ((Maybe (Int, Int), [LocalFunDecl (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn76 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn76 #-}
happyOut76 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [LocalFunDecl (Maybe (Int, Int))]))
happyOut76 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut76 #-}
happyIn77 :: ((Maybe (Int, Int), TypeSignature (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn77 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn77 #-}
happyOut77 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), TypeSignature (Maybe (Int, Int))))
happyOut77 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut77 #-}
happyIn78 :: ((Maybe (Int, Int), TypeDecl (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn78 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn78 #-}
happyOut78 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), TypeDecl (Maybe (Int, Int))))
happyOut78 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut78 #-}
happyIn79 :: ((Maybe (Int, Int), TypeParameter (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn79 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn79 #-}
happyOut79 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), TypeParameter (Maybe (Int, Int))))
happyOut79 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut79 #-}
happyIn80 :: ((Maybe (Int, Int), [TypeParameter (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn80 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn80 #-}
happyOut80 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [TypeParameter (Maybe (Int, Int))]))
happyOut80 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut80 #-}
happyIn81 :: ((Maybe (Int, Int), TypeVariantDecl (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn81 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn81 #-}
happyOut81 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), TypeVariantDecl (Maybe (Int, Int))))
happyOut81 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut81 #-}
happyIn82 :: ((Maybe (Int, Int), [TypeVariantDecl (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn82 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn82 #-}
happyOut82 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [TypeVariantDecl (Maybe (Int, Int))]))
happyOut82 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut82 #-}
happyIn83 :: ((Maybe (Int, Int), TypeBody (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn83 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn83 #-}
happyOut83 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), TypeBody (Maybe (Int, Int))))
happyOut83 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut83 #-}
happyIn84 :: ((Maybe (Int, Int), FieldDecl (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn84 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn84 #-}
happyOut84 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), FieldDecl (Maybe (Int, Int))))
happyOut84 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut84 #-}
happyIn85 :: ((Maybe (Int, Int), [FieldDecl (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn85 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn85 #-}
happyOut85 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [FieldDecl (Maybe (Int, Int))]))
happyOut85 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut85 #-}
happyIn86 :: ((Maybe (Int, Int), MemberDecl (Maybe (Int, Int)))) -> (HappyAbsSyn )
happyIn86 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn86 #-}
happyOut86 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), MemberDecl (Maybe (Int, Int))))
happyOut86 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut86 #-}
happyIn87 :: ((Maybe (Int, Int), [MemberDecl (Maybe (Int, Int))])) -> (HappyAbsSyn )
happyIn87 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn87 #-}
happyOut87 :: (HappyAbsSyn ) -> ((Maybe (Int, Int), [MemberDecl (Maybe (Int, Int))]))
happyOut87 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut87 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x20\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x01\x01\x48\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x01\x01\x48\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x10\x10\x80\x44\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x01\x00\x08\x20\x14\x1e\xc8\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x20\x00\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x01\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xac\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x02\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x80\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x42\xe0\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x20\x00\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x42\xe0\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x40\xf3\x84\x58\xee\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x01\x01\x48\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x01\x01\x48\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x01\x01\x48\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x12\x40\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x34\x4f\x88\xe5\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x21\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x04\x02\x80\x05\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x42\xe0\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x40\x20\x00\x58\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x80\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x01\x00\x08\x20\x14\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x20\x80\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x01\x00\x08\x20\x14\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x80\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x20\x00\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x40\x40\x20\x00\x58\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x02\xe0\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x00\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x01\x00\x00\x20\x14\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x00\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x01\x00\x00\x20\x14\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x00\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x01\x00\x00\x20\x14\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x00\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x01\x00\x00\x20\x14\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x00\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x20\x04\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x42\xe0\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x20\x04\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x42\xe0\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x20\x04\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x42\xe0\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x20\x04\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x20\x00\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x01\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x10\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\xac\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\xc0\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x20\x00\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x80\x00\x42\xe1\x81\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x40\x20\x00\x58\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x20\x00\x00\x02\xe0\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x80\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x80\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x01\x00\x08\x20\x14\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x80\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x01\x00\x08\x20\x14\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x80\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x01\x00\x08\x20\x14\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x80\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x40\x20\x00\x58\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x80\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x04\x02\x80\x05\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x80\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x04\x02\x80\x05\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x10\x00\x80\x00\x42\xe1\x01\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x01\x00\x08\x20\x14\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x40\x20\x00\x58\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x01\x00\x08\x20\x14\x1e\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x04\x02\x80\x05\x7c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x80\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram_internal","Ident","Integer","Char","String","UIdent","Program","TopLvlDecl","ListTopLvlDecl","TypeHint","TypeExpr2","TypeExpr1","TypeExpr","ListTypeExpr2","TupleType","TypePurity","FieldTypeExpr","ListFieldTypeExpr","FunDecl","FunParam","ListFunParam","LambdaParam","ListLambdaParam","FunBody","BoolLiteral","Literal","MemberAccess","ListMemberAccess","Expression13","Expression12","Expression11","Expression10","Expression9","Expression8","Expression7","Expression6","Expression5","Expression4","Expression3","Expression2","Expression1","Expression","TupleExpression","MatchExpressionClause","ListMatchExpressionClause","FieldAss","ListFieldAss","Statement5","Statement4","Statement3","Statement2","Statement1","Statement","ListStatement","MatchStatementClause","ListMatchStatementClause","ConditionalStatement","IfStatement","ElseIfStatement","ElseStatement","ListElseIfStatement","Pattern2","Pattern1","Pattern","TuplePattern","FieldPattern","ListFieldPattern","Declaration","AdHocFieldDecl","ListAdHocFieldDecl","LocalFunDecl","LocalObjDecl1","LocalObjDecl","ListLocalFunDecl","TypeSignature","TypeDecl","TypeParameter","ListTypeParameter","TypeVariantDecl","ListTypeVariantDecl","TypeBody","FieldDecl","ListFieldDecl","MemberDecl","ListMemberDecl","'!='","'('","'()'","')'","'*'","'*='","'+'","'+='","','","'-'","'-='","'->'","'.'","'/'","'/='","':'","'::'","':='","';'","'<'","'<='","'='","'=='","'=>'","'>'","'>='","'@'","'@='","'\\\\'","'^'","'^='","'_'","'and'","'break'","'continue'","'data'","'else'","'eval'","'false'","'for'","'if'","'impure'","'in'","'match'","'mod'","'not'","'or'","'ref'","'return'","'sideeffect'","'this'","'this.data'","'true'","'val'","'value'","'var'","'variant'","'where'","'while'","'yield'","'{'","'|'","'}'","L_ident","L_integ","L_charac","L_quoted","L_UIdent","%eof"]
        bit_start = st * 156
        bit_end = (st + 1) * 156
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..155]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\xeb\xff\x00\x00\xef\xff\x5c\x00\x29\x00\x3f\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xfe\xff\x00\x00\x44\x00\x00\x00\x54\x00\x00\x00\x09\x00\x5b\x01\x00\x00\x5b\x01\x00\x00\x7e\x00\x00\x00\x00\x00\x5b\x01\x00\x00\x00\x00\x00\x00\x52\x00\x00\x00\x00\x00\xe4\x01\x57\x00\x67\x00\x7c\x00\x00\x00\x00\x00\x17\x00\x94\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc6\x00\x00\x00\x00\x00\x00\x00\xc6\x00\x00\x00\x00\x00\x00\x00\xb6\x02\xbb\x00\xbc\x00\x12\x00\x78\x00\x00\x00\xad\x01\xf7\xff\x00\x00\x00\x00\x00\x00\x3e\x02\x00\x00\x7a\x02\x00\x00\x00\x00\xb6\x02\x7a\x02\x00\x00\xcc\x00\x00\x00\xfc\xff\x94\x01\x00\x00\x00\x00\x00\x00\xd6\x00\x00\x00\xdf\x00\xaf\x00\x84\x00\xeb\x00\x5b\x01\x00\x00\x5b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5b\x01\x00\x00\xb0\x00\xbb\x02\xb4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbc\x01\xe4\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdc\x00\x00\x00\xe0\x00\x00\x00\xc6\x02\x00\x00\xe1\x00\xea\x00\x7a\x02\xc6\x02\x3e\x02\x3e\x02\x02\x02\xbf\x00\x0e\x00\x3e\x02\x3e\x02\x00\x00\xc1\x00\x00\x00\xf2\x00\x00\x00\xc2\x00\xcc\x02\xc7\x00\x73\x02\xcc\x02\xfa\x00\x01\x01\x02\x01\x5c\x02\x5c\x02\x5c\x02\x5c\x02\x5c\x02\x5c\x02\x5c\x02\x5c\x02\x5c\x02\xa0\x02\xa0\x02\xa0\x02\xa0\x02\xa0\x02\xa0\x02\xa0\x02\x00\x00\x00\x00\x00\x00\xcd\x00\xca\x00\xf5\x00\xfd\x00\xd7\x00\xd8\x00\xd1\x00\x03\x01\x00\x00\x00\x00\xe6\x00\xdb\x00\x00\x00\x00\x00\x0d\x01\x00\x00\xcc\x02\x06\x01\x04\x01\x04\x01\x04\x01\x12\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xad\x01\xad\x01\x00\x00\x00\x00\xcc\x02\x00\x00\x00\x00\x00\x00\x00\x00\x20\x02\xf2\x02\x00\x00\x16\x01\x25\x00\xe4\x00\x15\x01\xec\x00\x00\x00\xf8\x00\xf8\x00\x17\x01\x00\x00\xfe\x00\xff\x00\x0f\x01\x34\x01\x00\x00\x00\x00\x2e\x01\x3c\x01\x38\x01\x44\x01\x00\x00\x3e\x02\x24\x01\x0a\x01\x00\x00\x00\x00\x0a\x01\x3e\x02\x3e\x02\x3e\x02\x3e\x02\x3e\x02\x3e\x02\x3e\x02\x00\x00\x43\x01\x00\x00\x3a\x01\x40\x01\x41\x01\x4c\x01\x4f\x01\x50\x01\x51\x01\x3b\x01\x67\x01\x32\x01\x00\x00\x00\x00\x5f\x01\x3d\x01\x00\x00\x00\x00\xf0\xff\x60\x01\x00\x00\xf2\x02\x00\x00\x00\x00\x3e\x02\x00\x00\xf2\x02\x00\x00\x3e\x01\x00\x00\x00\x00\x3f\x01\x70\x01\x47\x01\x64\x01\x68\x01\x00\x00\x00\x00\x7e\x01\x52\x01\x76\x01\x00\x00\x86\x01\x00\x00\x00\x00\x53\x01\x00\x00\x55\x01\x75\x01\x00\x00\x87\x01\x5d\x01\x3e\x02\x00\x00\xf2\x02\x00\x00\x3e\x02\x00\x00\x5e\x01\x3e\x02\x00\x00\xf2\x02\x61\x01\x85\x01\x65\x01\x9a\x01\x00\x00\x00\x00\x00\x00\x3e\x02\x00\x00\x66\x01\x00\x00\x66\x01\xf2\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x73\x01\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x78\x01\x74\x01\x00\x00\x00\x00\x79\x01\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x79\x00\x00\x00\x00\x00\x00\x00\x05\x00\x91\x01\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\x03\x00\x69\x01\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x00\xa1\x01\x00\x00\x80\x07\x00\x00\x00\x00\x00\x00\x00\x00\x50\x03\x00\x00\x00\x00\x00\x00\x81\x00\x00\x00\x00\x00\x21\x03\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x91\x00\xf2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x59\x00\x00\x00\x00\x00\x00\x00\xa1\x00\x00\x00\x00\x00\x00\x00\x95\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x73\x03\x00\x00\x26\x07\xae\x01\x00\x00\x14\x03\x46\x07\x00\x00\xaa\x00\x00\x00\xc4\x01\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7d\x07\x00\x00\x8d\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x03\x00\x00\xc8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x77\x00\x00\x00\x00\x00\x90\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x83\x00\x00\x00\x00\x00\x00\x00\x4f\x07\x4e\x01\xac\x03\xbe\x03\xe7\x03\x00\x00\x0f\x00\xf9\x03\x22\x04\x00\x00\x00\x00\x89\x01\x00\x00\x00\x00\xce\x01\x78\x07\x00\x00\x1a\x01\x78\x07\x00\x00\x00\x00\x00\x00\xd0\x05\xf6\x05\xbf\x05\x05\x06\x2a\x06\x38\x06\x5d\x06\x6b\x06\x90\x06\x9e\x06\xc1\x06\xcd\x06\xef\x06\xfa\x06\x1c\x07\x58\x07\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd5\x01\x00\x00\x00\x00\x00\x00\xd4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x78\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9d\x03\x00\x00\x00\x00\x00\x00\x00\x00\x4a\x03\xcb\x00\x00\x00\xad\x00\x08\x00\x0a\x00\x00\x00\xa4\x01\x00\x00\x15\x00\x28\x00\x00\x00\x00\x00\x00\x00\xac\x01\x00\x00\x78\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x04\x64\x00\x13\x00\x00\x00\x00\x00\x2e\x00\x5d\x04\x6f\x04\x98\x04\xaa\x04\xd3\x04\xe5\x04\x0e\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xaf\x01\x00\x00\x00\x00\x42\x01\x00\x00\x00\x00\x20\x05\x00\x00\x9f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x21\x00\x00\x00\x00\x00\x00\x00\x49\x05\x00\x00\xe9\x00\x00\x00\x5b\x05\x00\x00\x11\x00\x84\x05\x00\x00\xf7\x00\x00\x00\x00\x00\xb2\x01\x00\x00\x00\x00\x00\x00\x00\x00\x96\x05\x00\x00\x14\x00\x00\x00\x3c\x00\x6b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb3\x01\x00\x00\xa3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x01\x06\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xf5\xff\x00\x00\xfe\xff\x00\x00\xf9\xff\xdb\xff\x00\x00\xf7\xff\xf8\xff\xf6\xff\x00\x00\x00\x00\x4f\xff\x00\x00\xfa\xff\x00\x00\xf4\xff\x00\x00\x00\x00\xf2\xff\xf1\xff\xea\xff\xe8\xff\xf3\xff\xf0\xff\x00\x00\xef\xff\xe3\xff\xe2\xff\xe0\xff\xdc\xff\xda\xff\x00\x00\x00\x00\x00\x00\x54\xff\x50\xff\x4e\xff\x43\xff\x43\xff\xc7\xff\xd1\xff\xd0\xff\xcf\xff\x00\x00\xdd\xff\xce\xff\xc8\xff\xc2\xff\xbf\xff\xbd\xff\xbb\xff\xb9\xff\xb7\xff\xb3\xff\xb0\xff\xad\xff\xa6\xff\xa3\xff\xa1\xff\x9f\xff\xd6\xff\xd5\xff\x00\x00\xd2\xff\x00\x00\xd8\xff\xd3\xff\x00\x00\x00\x00\xca\xff\x00\x00\xd4\xff\x00\x00\x00\x00\xfd\xff\xfc\xff\xfb\xff\x00\x00\xe1\xff\xdf\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf1\xff\xe7\xff\xeb\xff\xe6\xff\xe9\xff\xee\xff\xec\xff\x00\x00\xed\xff\xe0\xff\x60\xff\x00\x00\x6e\xff\x8d\xff\x88\xff\x85\xff\x7d\xff\x7b\xff\x7a\xff\x00\x00\x89\xff\x70\xff\x6d\xff\x68\xff\x00\x00\x57\xff\x58\xff\x6c\xff\x00\x00\x69\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x95\xff\x00\x00\x5d\xff\xcc\xff\xc3\xff\x00\x00\xaf\xff\x00\x00\x00\x00\xae\xff\xbd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbc\xff\xc4\xff\xc5\xff\x00\x00\x49\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x53\xff\x52\xff\x4c\xff\x00\x00\x45\xff\x44\xff\x00\x00\x51\xff\xba\xff\xb8\xff\xb4\xff\xb5\xff\xb6\xff\xb1\xff\xb2\xff\xa7\xff\xa9\xff\xac\xff\xa8\xff\xaa\xff\xab\xff\xa2\xff\xa4\xff\xa5\xff\xc9\xff\xc6\xff\x00\x00\x60\xff\xd7\xff\xd9\xff\x58\xff\x00\x00\x00\x00\xcd\xff\x00\x00\x00\x00\x98\xff\x00\x00\x00\x00\x59\xff\x00\x00\x63\xff\x00\x00\x92\xff\x00\x00\x00\x00\x00\x00\x00\x00\x90\xff\x8f\xff\x60\xff\x00\x00\x00\x00\x00\x00\x86\xff\x00\x00\x75\xff\x00\x00\x94\xff\x79\xff\x63\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xde\xff\xe4\xff\xe5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x62\xff\x00\x00\x5b\xff\x5a\xff\x00\x00\x00\x00\x6f\xff\x74\xff\x00\x00\x00\x00\x6a\xff\x00\x00\x5f\xff\x7c\xff\x00\x00\x73\xff\x00\x00\x91\xff\x00\x00\x8b\xff\x8e\xff\x00\x00\x97\xff\x00\x00\x00\x00\x00\x00\xc1\xff\xcb\xff\x9b\xff\x00\x00\x00\x00\xa0\xff\x9d\xff\x9e\xff\x42\xff\x00\x00\x4b\xff\x00\x00\x43\xff\x48\xff\x00\x00\x00\x00\x00\x00\xbe\xff\x00\x00\x5c\xff\x00\x00\xc0\xff\x98\xff\x00\x00\x6b\xff\x77\xff\x00\x00\x00\x00\x00\x00\x65\xff\x66\xff\x87\xff\x71\xff\x00\x00\x93\xff\x56\xff\x67\xff\x63\xff\x00\x00\x81\xff\x83\xff\x82\xff\x80\xff\x84\xff\x7e\xff\x7f\xff\x64\xff\x61\xff\x55\xff\x00\x00\x8a\xff\x00\x00\x8c\xff\x76\xff\x99\xff\x96\xff\x5e\xff\x9a\xff\x9c\xff\x43\xff\x47\xff\x00\x00\x4d\xff\x46\xff\x4a\xff\x78\xff\x72\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x08\x00\x00\x00\x06\x00\x08\x00\x08\x00\x08\x00\x00\x00\x02\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x11\x00\x05\x00\x21\x00\x29\x00\x00\x00\x08\x00\x08\x00\x08\x00\x3a\x00\x16\x00\x0e\x00\x4d\x00\x4e\x00\x3f\x00\x11\x00\x11\x00\x2f\x00\x02\x00\x00\x00\x22\x00\x23\x00\x40\x00\x12\x00\x3d\x00\x00\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x45\x00\x3e\x00\x2c\x00\x2d\x00\x31\x00\x3d\x00\x11\x00\x24\x00\x00\x00\x2c\x00\x2d\x00\x2d\x00\x44\x00\x4f\x00\x44\x00\x3c\x00\x3d\x00\x53\x00\x17\x00\x18\x00\x49\x00\x40\x00\x42\x00\x43\x00\x49\x00\x4b\x00\x40\x00\x4a\x00\x39\x00\x42\x00\x13\x00\x52\x00\x50\x00\x51\x00\x50\x00\x51\x00\x45\x00\x45\x00\x16\x00\x48\x00\x48\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x3f\x00\x40\x00\x37\x00\x38\x00\x40\x00\x41\x00\x16\x00\x3c\x00\x3d\x00\x3e\x00\x40\x00\x41\x00\x4f\x00\x42\x00\x19\x00\x1a\x00\x53\x00\x46\x00\x47\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x40\x00\x41\x00\x05\x00\x07\x00\x07\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x04\x00\x08\x00\x0c\x00\x08\x00\x30\x00\x09\x00\x17\x00\x18\x00\x0f\x00\x10\x00\x40\x00\x37\x00\x3d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x17\x00\x18\x00\x40\x00\x39\x00\x3a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x3d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x17\x00\x18\x00\x37\x00\x38\x00\x1b\x00\x1c\x00\x1d\x00\x3c\x00\x3d\x00\x3e\x00\x17\x00\x18\x00\x24\x00\x42\x00\x19\x00\x1a\x00\x40\x00\x46\x00\x47\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x19\x00\x1a\x00\x42\x00\x19\x00\x1a\x00\x00\x00\x46\x00\x47\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x08\x00\x2e\x00\x2f\x00\x0d\x00\x35\x00\x36\x00\x1b\x00\x0f\x00\x10\x00\x0d\x00\x1e\x00\x3c\x00\x3d\x00\x3e\x00\x4d\x00\x4e\x00\x4f\x00\x42\x00\x17\x00\x18\x00\x53\x00\x46\x00\x47\x00\x11\x00\x09\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x3f\x00\x04\x00\x40\x00\x3d\x00\x16\x00\x13\x00\x13\x00\x2a\x00\x2b\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x3d\x00\x13\x00\x3d\x00\x0d\x00\x17\x00\x18\x00\x40\x00\x09\x00\x3d\x00\x04\x00\x04\x00\x3c\x00\x3d\x00\x3e\x00\x40\x00\x16\x00\x3f\x00\x42\x00\x17\x00\x18\x00\x13\x00\x46\x00\x47\x00\x2a\x00\x2b\x00\x44\x00\x3f\x00\x3f\x00\x3d\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x39\x00\x13\x00\x1b\x00\x1e\x00\x0d\x00\x40\x00\x3c\x00\x3d\x00\x3e\x00\x13\x00\x3d\x00\x13\x00\x42\x00\x35\x00\x36\x00\x14\x00\x46\x00\x47\x00\x17\x00\x18\x00\x3c\x00\x3d\x00\x3e\x00\x02\x00\x03\x00\x40\x00\x42\x00\x2b\x00\x3d\x00\x3d\x00\x46\x00\x47\x00\x11\x00\x04\x00\x09\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x13\x00\x04\x00\x25\x00\x40\x00\x10\x00\x09\x00\x13\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x13\x00\x13\x00\x16\x00\x3c\x00\x3d\x00\x3e\x00\x17\x00\x18\x00\x27\x00\x42\x00\x02\x00\x03\x00\x13\x00\x46\x00\x47\x00\x13\x00\x13\x00\x13\x00\x17\x00\x18\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x09\x00\x3f\x00\x13\x00\x13\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x09\x00\x16\x00\x13\x00\x3f\x00\x3f\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x17\x00\x18\x00\x42\x00\x2a\x00\x3f\x00\x09\x00\x46\x00\x47\x00\x3c\x00\x3d\x00\x3e\x00\x32\x00\x18\x00\x09\x00\x42\x00\x3f\x00\x3d\x00\x40\x00\x46\x00\x47\x00\x02\x00\x03\x00\x3d\x00\x24\x00\x13\x00\x40\x00\x3f\x00\x18\x00\x40\x00\x44\x00\x3f\x00\x00\x00\x3d\x00\x09\x00\x13\x00\x04\x00\x40\x00\x3c\x00\x3d\x00\x3e\x00\x09\x00\x0a\x00\x0b\x00\x42\x00\x01\x00\x0e\x00\x3d\x00\x46\x00\x47\x00\x3f\x00\x20\x00\x4c\x00\x22\x00\x23\x00\x40\x00\x40\x00\x26\x00\x27\x00\x28\x00\x29\x00\x02\x00\x03\x00\x2c\x00\x14\x00\x15\x00\x15\x00\x17\x00\x31\x00\x19\x00\x1a\x00\x04\x00\x35\x00\x36\x00\x3b\x00\x38\x00\x44\x00\x00\x00\x3b\x00\x3c\x00\x3d\x00\x2e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x04\x00\x2e\x00\x53\x00\x20\x00\x2e\x00\x22\x00\x23\x00\x2e\x00\x2e\x00\x26\x00\x27\x00\x28\x00\x29\x00\x02\x00\x03\x00\x2c\x00\xff\xff\xff\xff\xff\xff\xff\xff\x31\x00\x0a\x00\xff\xff\xff\xff\x35\x00\x36\x00\xff\xff\x38\x00\xff\xff\xff\xff\x3b\x00\x3c\x00\x3d\x00\xff\xff\xff\xff\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x1d\x00\xff\xff\xff\xff\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x27\x00\x0a\x00\xff\xff\xff\xff\xff\xff\x2c\x00\xff\xff\x2e\x00\xff\xff\xff\xff\x13\x00\xff\xff\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\x3d\x00\x02\x00\x03\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x27\x00\x0a\x00\xff\xff\xff\xff\xff\xff\x2c\x00\xff\xff\x2e\x00\xff\xff\xff\xff\xff\xff\xff\xff\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x02\x00\x03\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x27\x00\x0a\x00\xff\xff\xff\xff\xff\xff\x2c\x00\xff\xff\x2e\x00\xff\xff\xff\xff\xff\xff\xff\xff\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\xff\xff\xff\xff\xff\xff\x1d\x00\xff\xff\x3d\x00\x02\x00\x03\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x27\x00\x0a\x00\xff\xff\xff\xff\xff\xff\x2c\x00\xff\xff\x2e\x00\xff\xff\xff\xff\xff\xff\xff\xff\x33\x00\x34\x00\x35\x00\x36\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x02\x00\x03\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2c\x00\xff\xff\x2e\x00\x18\x00\xff\xff\xff\xff\xff\xff\x33\x00\x34\x00\x35\x00\x36\x00\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x27\x00\xff\xff\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x27\x00\x02\x00\x03\x00\xff\xff\xff\xff\x2c\x00\xff\xff\x35\x00\x36\x00\xff\xff\x38\x00\xff\xff\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\xff\xff\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x02\x00\x03\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\xff\xff\x06\x00\xff\xff\x08\x00\xff\xff\xff\xff\x0b\x00\x27\x00\x02\x00\x03\x00\x0f\x00\xff\xff\x2c\x00\x12\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\x33\x00\x34\x00\x35\x00\x36\x00\x1c\x00\xff\xff\xff\xff\x1f\x00\xff\xff\xff\xff\x27\x00\xff\xff\xff\xff\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x20\x00\xff\xff\xff\xff\x33\x00\x34\x00\x35\x00\x36\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x27\x00\x02\x00\x03\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x35\x00\x36\x00\xff\xff\x38\x00\x33\x00\x34\x00\x35\x00\x36\x00\xff\xff\xff\xff\xff\xff\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\xff\xff\x20\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\x35\x00\x36\x00\xff\xff\x38\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x16\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x2e\x00\x00\x00\xff\xff\xff\xff\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\xff\xff\x0d\x00\x0e\x00\xff\xff\x16\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x2e\x00\x00\x00\xff\xff\xff\xff\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\xff\xff\x0d\x00\x0e\x00\xff\xff\xff\xff\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x17\x00\x18\x00\xff\xff\x29\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x17\x00\x18\x00\xff\xff\xff\xff\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\xff\xff\xff\xff\x00\x00\x04\x00\xff\xff\xff\xff\x04\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x09\x00\xff\xff\x0e\x00\x0c\x00\x00\x00\x0e\x00\x17\x00\x18\x00\x04\x00\xff\xff\x1b\x00\x1c\x00\x1d\x00\x09\x00\xff\xff\xff\xff\x0c\x00\xff\xff\x0e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x05\x00\x24\x00\x0c\x00\x4e\x00\x05\x00\x4e\x00\x0c\x00\xc1\x00\xaa\x00\x0a\x01\x06\x00\x1c\x01\x07\x00\x1c\x01\xc1\x00\xcf\x00\x0a\x01\xab\x00\x05\x00\x05\x00\x4e\x00\x08\x00\x97\x00\x8c\x00\x31\x01\x1e\x00\xf7\x00\xf7\x00\xd9\x00\xe0\x00\x21\x00\x98\x00\xa1\x00\x19\x01\xe1\x00\xf8\x00\xf8\x00\x8d\x00\xcf\x00\xf4\x00\x74\x00\x75\x00\x03\x00\x1f\x00\x4b\x00\xf4\x00\x5f\x00\x29\x00\x2a\x00\x2b\x00\x60\x00\xff\xff\x8e\x00\x0b\x01\x0c\x01\x7a\x00\x81\x00\x13\x00\xa1\x00\xf4\x00\x0b\x01\x46\x01\x99\x00\x0f\x00\x9e\x00\x0f\x00\x7e\x00\x4b\x00\x9f\x00\x2e\x00\x61\x00\x0f\x00\x03\x00\x0d\x01\x0e\x01\x0d\x00\x25\x00\x03\x00\x09\x00\xa5\x00\xcd\x00\x11\x00\xac\x00\x1d\x01\x1e\x01\x1d\x01\x4e\x01\xf9\x00\xf9\x00\x23\x00\xfa\x00\x3f\x01\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x10\x01\x03\x00\x69\x00\x6a\x00\xf5\x00\x07\x01\x22\x00\x6b\x00\x6c\x00\x6d\x00\xf5\x00\xf6\x00\x4c\x01\x6e\x00\x81\x00\x9d\x00\x9f\x00\x6f\x00\x70\x00\x5f\x00\x29\x00\x2a\x00\x2b\x00\x60\x00\xf5\x00\x3e\x01\x03\x00\x95\x00\x04\x00\x4e\x00\x96\x00\xd8\x00\x29\x00\x2a\x00\x2b\x00\x60\x00\x5c\x00\x4f\x00\x55\x00\xd9\x00\x0b\x00\x5d\x00\x2e\x00\x61\x00\x50\x00\x51\x00\x03\x00\x0c\x00\x28\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2e\x00\x61\x00\x03\x00\xfb\x00\xfc\x00\xc1\x00\x29\x00\x2a\x00\x2b\x00\x60\x00\x27\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\xe1\x00\x2e\x00\x2f\x00\x69\x00\x6a\x00\x30\x00\x31\x00\x9b\x00\x6b\x00\x6c\x00\x6d\x00\x2e\x00\x61\x00\xa1\x00\x6e\x00\x81\x00\x9c\x00\x03\x00\x6f\x00\x70\x00\x6b\x00\x6c\x00\xda\x00\xdb\x00\x81\x00\x82\x00\x6e\x00\x81\x00\x10\x01\x4e\x00\xc4\x00\x70\x00\xc1\x00\x29\x00\x2a\x00\x2b\x00\x60\x00\x4f\x00\x62\x00\x50\x01\x84\x00\x28\x01\x29\x01\x9b\x00\x50\x00\xea\x00\x84\x00\x9a\x00\x6b\x00\x6c\x00\x2a\x01\xa1\x00\xa2\x00\xa3\x00\x6e\x00\x2e\x00\x61\x00\x9f\x00\xc4\x00\x70\x00\x13\x00\x5f\x00\xc1\x00\x29\x00\x2a\x00\x2b\x00\x60\x00\x5e\x00\x5b\x00\x03\x00\xe3\x00\xde\x00\xdd\x00\xd8\x00\x11\x01\x12\x01\xc1\x00\x29\x00\x2a\x00\x2b\x00\x60\x00\xd0\x00\xd7\x00\xcb\x00\xc9\x00\x2e\x00\x61\x00\x03\x00\xc1\x00\xc7\x00\xc0\x00\xbf\x00\x6b\x00\x6c\x00\x13\x01\x03\x00\xaa\x00\xae\x00\x6e\x00\x2e\x00\x61\x00\xa9\x00\xc4\x00\x70\x00\x11\x01\x48\x01\x0f\x00\xa8\x00\xa7\x00\x19\x01\x1b\x01\xc1\x00\x29\x00\x2a\x00\x2b\x00\x60\x00\xa5\x00\x18\x01\x9b\x00\x9a\x00\x84\x00\x03\x00\x6b\x00\x6c\x00\x13\x01\x0a\x01\x4b\x00\x07\x01\x6e\x00\x28\x01\x44\x01\xc2\x00\xc4\x00\x70\x00\x2e\x00\x61\x00\x6b\x00\x6c\x00\x2a\x01\x40\x00\x41\x00\x03\x00\x6e\x00\x04\x01\x06\x01\x4b\x00\xc4\x00\x70\x00\x13\x00\x02\x01\x01\x01\xc1\x00\x29\x00\x2a\x00\x2b\x00\x60\x00\x03\x01\x00\x01\xfe\x00\x03\x00\x36\x01\x5d\x00\x3d\x01\xc1\x00\x29\x00\x2a\x00\x2b\x00\x60\x00\x3c\x01\x3b\x01\x27\x01\x6b\x00\x6c\x00\xc3\x00\x2e\x00\x61\x00\x44\x00\x6e\x00\x1a\x00\x1b\x00\x3a\x01\xc4\x00\x70\x00\x39\x01\x38\x01\x37\x01\x2e\x00\x61\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xc1\x00\x29\x00\x2a\x00\x2b\x00\x60\x00\x35\x01\x34\x01\x33\x01\x2f\x01\x03\x00\x4c\x00\x4d\x00\x4e\x00\x0f\x00\x26\x01\x24\x01\x23\x01\x32\x01\x28\x01\x6b\x00\x6c\x00\x2c\x01\x2d\x01\x2e\x00\x61\x00\x6e\x00\x1c\x00\x25\x01\x22\x01\xc4\x00\x70\x00\x6b\x00\x6c\x00\xd4\x00\x1d\x00\x20\x01\xc1\x00\x6e\x00\x21\x01\x1c\x01\x03\x00\xc4\x00\x70\x00\x72\x00\x41\x00\x1e\x00\xa1\x00\x4c\x01\x03\x00\x4b\x01\x43\x01\x03\x00\x0f\x00\x44\x01\x13\x00\x4b\x00\x01\x01\x11\x00\x14\x00\x03\x00\x6b\x00\x6c\x00\x3d\x01\x15\x00\x16\x00\x17\x00\x6e\x00\x8f\x00\x18\x00\x4b\x00\xc4\x00\x70\x00\x4e\x01\x73\x00\x23\x00\x74\x00\x75\x00\x03\x00\x03\x00\x76\x00\x44\x00\x77\x00\x78\x00\x72\x00\x41\x00\x79\x00\x90\x00\x91\x00\x86\x00\x92\x00\x7a\x00\x93\x00\x94\x00\x7f\x00\x49\x00\x7b\x00\xde\x00\x7c\x00\xc9\x00\xc7\x00\x7d\x00\x7e\x00\x4b\x00\x08\x01\x7f\x00\x03\x00\x4c\x00\x4d\x00\x4e\x00\x0f\x00\xa5\x00\x04\x01\x4f\x01\x73\x00\x2f\x01\x74\x00\x75\x00\x41\x01\x51\x01\x76\x00\x44\x00\x77\x00\x78\x00\x40\x00\x41\x00\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7a\x00\x42\x00\x00\x00\x00\x00\x49\x00\x7b\x00\x00\x00\x7c\x00\x00\x00\x00\x00\x7d\x00\x7e\x00\x4b\x00\x00\x00\x00\x00\x03\x00\x4c\x00\x4d\x00\x4e\x00\x0f\x00\x43\x00\x00\x00\x00\x00\x40\x00\x41\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x42\x00\x00\x00\x00\x00\x00\x00\x45\x00\x00\x00\x46\x00\x00\x00\x00\x00\xd2\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x43\x00\x00\x00\x4b\x00\x40\x00\x41\x00\x03\x00\x4c\x00\x4d\x00\x4e\x00\x0f\x00\x44\x00\x42\x00\x00\x00\x00\x00\x00\x00\x45\x00\x00\x00\x46\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x43\x00\x00\x00\x00\x00\x40\x00\x41\x00\x03\x00\x4c\x00\x4d\x00\x4e\x00\x0f\x00\x44\x00\x42\x00\x00\x00\x00\x00\x00\x00\x45\x00\x00\x00\x46\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x43\x00\x00\x00\x4b\x00\x40\x00\x41\x00\x03\x00\x4c\x00\x4d\x00\x4e\x00\x0f\x00\x44\x00\x42\x00\x00\x00\x00\x00\x00\x00\x45\x00\x00\x00\x46\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x72\x00\x41\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x41\x00\x03\x00\x4c\x00\x4d\x00\x4e\x00\x0f\x00\x44\x00\x00\x00\x00\x00\x00\x00\x00\x00\x45\x00\x00\x00\x46\x00\xc6\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x73\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x03\x00\x4c\x00\x4d\x00\x4e\x00\x0f\x00\x44\x00\x40\x00\x41\x00\x00\x00\x00\x00\x45\x00\x00\x00\x49\x00\x7b\x00\x00\x00\x7c\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x03\x00\x4c\x00\x4d\x00\x4e\x00\x0f\x00\x40\x00\x41\x00\x03\x00\x4c\x00\x4d\x00\x4e\x00\x0f\x00\x00\x00\x00\x00\xe4\x00\x00\x00\xe5\x00\x00\x00\x00\x00\xe6\x00\x44\x00\x72\x00\x41\x00\xe7\x00\x00\x00\x45\x00\xe8\x00\x40\x00\x41\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xe9\x00\x00\x00\x00\x00\xea\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x03\x00\x4c\x00\x4d\x00\x4e\x00\x0f\x00\x00\x00\x73\x00\x00\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x44\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x72\x00\x41\x00\x03\x00\x4c\x00\x4d\x00\x4e\x00\x0f\x00\x49\x00\x7b\x00\x00\x00\x7c\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x03\x00\x4c\x00\x4d\x00\x4e\x00\x0f\x00\x00\x00\x03\x00\x4c\x00\x4d\x00\x4e\x00\x0f\x00\x00\x00\x73\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x44\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x49\x00\x7b\x00\x00\x00\x7c\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x85\x00\x03\x00\x4c\x00\x4d\x00\x4e\x00\x0f\x00\x2d\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x3e\x00\x13\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x15\x00\x16\x00\x52\x00\x00\x00\x53\x00\x18\x00\x00\x00\x14\x01\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x3e\x00\x13\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x15\x00\x16\x00\xeb\x00\x00\x00\xec\x00\x18\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x88\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x89\x00\x8a\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x15\x01\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2e\x00\x2f\x00\x00\x00\x16\x01\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\xd3\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\xd2\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\xd0\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\xcc\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\xcb\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\xfe\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\xf3\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\xf2\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\xf1\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\xf0\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\xef\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\xee\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\xed\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x2b\x01\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x49\x01\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x47\x01\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x45\x01\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x40\x01\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\xbb\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xbd\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xbc\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\xba\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\xb9\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\xb8\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\xb7\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\xb6\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\xb5\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xb4\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\xb3\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\xb2\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\xb1\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\xb0\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\xaf\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x87\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\x84\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\xd5\x00\x2e\x00\x2f\x00\x00\x00\x00\x00\x30\x00\x31\x00\x32\x00\x33\x00\xae\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x13\x00\x00\x00\x00\x00\x13\x00\x14\x00\x00\x00\x00\x00\x55\x00\x00\x00\x15\x00\x16\x00\x59\x00\x56\x00\x00\x00\x18\x00\x57\x00\x13\x00\x18\x00\x2e\x00\x2f\x00\x55\x00\x00\x00\x30\x00\x31\x00\x9b\x00\x56\x00\x00\x00\x00\x00\x58\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 189) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134),
	(135 , happyReduce_135),
	(136 , happyReduce_136),
	(137 , happyReduce_137),
	(138 , happyReduce_138),
	(139 , happyReduce_139),
	(140 , happyReduce_140),
	(141 , happyReduce_141),
	(142 , happyReduce_142),
	(143 , happyReduce_143),
	(144 , happyReduce_144),
	(145 , happyReduce_145),
	(146 , happyReduce_146),
	(147 , happyReduce_147),
	(148 , happyReduce_148),
	(149 , happyReduce_149),
	(150 , happyReduce_150),
	(151 , happyReduce_151),
	(152 , happyReduce_152),
	(153 , happyReduce_153),
	(154 , happyReduce_154),
	(155 , happyReduce_155),
	(156 , happyReduce_156),
	(157 , happyReduce_157),
	(158 , happyReduce_158),
	(159 , happyReduce_159),
	(160 , happyReduce_160),
	(161 , happyReduce_161),
	(162 , happyReduce_162),
	(163 , happyReduce_163),
	(164 , happyReduce_164),
	(165 , happyReduce_165),
	(166 , happyReduce_166),
	(167 , happyReduce_167),
	(168 , happyReduce_168),
	(169 , happyReduce_169),
	(170 , happyReduce_170),
	(171 , happyReduce_171),
	(172 , happyReduce_172),
	(173 , happyReduce_173),
	(174 , happyReduce_174),
	(175 , happyReduce_175),
	(176 , happyReduce_176),
	(177 , happyReduce_177),
	(178 , happyReduce_178),
	(179 , happyReduce_179),
	(180 , happyReduce_180),
	(181 , happyReduce_181),
	(182 , happyReduce_182),
	(183 , happyReduce_183),
	(184 , happyReduce_184),
	(185 , happyReduce_185),
	(186 , happyReduce_186),
	(187 , happyReduce_187),
	(188 , happyReduce_188),
	(189 , happyReduce_189)
	]

happy_n_terms = 70 :: Int
happy_n_nonterms = 84 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn4
		 ((Just (tokenLineCol happy_var_1), Ident (prToken happy_var_1))
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ((Just (tokenLineCol happy_var_1), read (prToken happy_var_1))
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 ((Just (tokenLineCol happy_var_1), read (prToken happy_var_1))
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn7
		 ((Just (tokenLineCol happy_var_1), prToken happy_var_1)
	)}

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn8
		 ((Just (tokenLineCol happy_var_1), UIdent (prToken happy_var_1))
	)}

happyReduce_6 = happySpecReduce_1  5# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 ((fst happy_var_1, Harper.Abs.Prog (fst happy_var_1)(reverse (snd happy_var_1)))
	)}

happyReduce_7 = happySpecReduce_1  6# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 ((fst happy_var_1, Harper.Abs.TopLvlFDecl (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_8 = happySpecReduce_1  6# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 ((fst happy_var_1, Harper.Abs.TopLvlTHint (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_9 = happySpecReduce_1  6# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOut78 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 ((fst happy_var_1, Harper.Abs.TopLvlTDecl (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_10 = happySpecReduce_0  7# happyReduction_10
happyReduction_10  =  happyIn11
		 ((Nothing, [])
	)

happyReduce_11 = happySpecReduce_3  7# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	happyIn11
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_12 = happySpecReduce_3  8# happyReduction_12
happyReduction_12 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 ((fst happy_var_1, Harper.Abs.THint (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_13 = happySpecReduce_1  9# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 ((fst happy_var_1, Harper.Abs.TVar (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_14 = happySpecReduce_1  9# happyReduction_14
happyReduction_14 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 ((fst happy_var_1, Harper.Abs.TCtor (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_15 = happySpecReduce_1  9# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 ((fst happy_var_1, Harper.Abs.TPur (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_16 = happySpecReduce_1  9# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn13
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.TUnit (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_17 = happySpecReduce_3  9# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.TTup (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_18 = happySpecReduce_3  9# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.TAdHoc (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_19 = happySpecReduce_3  9# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 ((Just (tokenLineCol happy_var_1), snd happy_var_2)
	)}}

happyReduce_20 = happySpecReduce_2  10# happyReduction_20
happyReduction_20 happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn14
		 ((fst happy_var_1, Harper.Abs.TApp (fst happy_var_1)(snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_21 = happySpecReduce_1  10# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_22 = happySpecReduce_3  11# happyReduction_22
happyReduction_22 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 ((fst happy_var_1, Harper.Abs.TFun (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_23 = happySpecReduce_1  11# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_24 = happySpecReduce_1  12# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_25 = happySpecReduce_2  12# happyReduction_25
happyReduction_25 happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn16
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_26 = happySpecReduce_3  13# happyReduction_26
happyReduction_26 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 ((fst happy_var_1, Harper.Abs.TTupList (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_27 = happySpecReduce_3  13# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 ((fst happy_var_1, Harper.Abs.TTupTail (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_28 = happySpecReduce_1  14# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn18
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.TImpure (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_29 = happySpecReduce_1  14# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn18
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.TSideE (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_30 = happySpecReduce_1  15# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 ((fst happy_var_1, Harper.Abs.TFld (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_31 = happySpecReduce_0  16# happyReduction_31
happyReduction_31  =  happyIn20
		 ((Nothing, [])
	)

happyReduce_32 = happySpecReduce_1  16# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_33 = happySpecReduce_3  16# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_34 = happyReduce 4# 17# happyReduction_34
happyReduction_34 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_4 of { happy_var_4 -> 
	happyIn21
		 ((fst happy_var_1, Harper.Abs.FDecl (fst happy_var_1)(snd happy_var_1)(reverse (snd happy_var_2)) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_35 = happySpecReduce_1  18# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 ((fst happy_var_1, Harper.Abs.FParam (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_36 = happySpecReduce_0  19# happyReduction_36
happyReduction_36  =  happyIn23
		 ((Nothing, [])
	)

happyReduce_37 = happySpecReduce_2  19# happyReduction_37
happyReduction_37 happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_2 of { happy_var_2 -> 
	happyIn23
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_38 = happySpecReduce_1  20# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut66 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ((fst happy_var_1, Harper.Abs.LamParam (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_39 = happySpecReduce_0  21# happyReduction_39
happyReduction_39  =  happyIn25
		 ((Nothing, [])
	)

happyReduce_40 = happySpecReduce_2  21# happyReduction_40
happyReduction_40 happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_41 = happySpecReduce_1  22# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 ((fst happy_var_1, Harper.Abs.FExprBody (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_42 = happySpecReduce_1  22# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 ((fst happy_var_1, Harper.Abs.FStmtBody (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_43 = happySpecReduce_1  23# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn27
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.BTrue (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_44 = happySpecReduce_1  23# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn27
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.BFalse (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_45 = happySpecReduce_1  24# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn28
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.UnitLit (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_46 = happySpecReduce_1  24# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 ((fst happy_var_1, Harper.Abs.IntLit (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_47 = happySpecReduce_1  24# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 ((fst happy_var_1, Harper.Abs.CharLit (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_48 = happySpecReduce_1  24# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 ((fst happy_var_1, Harper.Abs.StrLit (fst happy_var_1)(init $ tail $ snd happy_var_1))
	)}

happyReduce_49 = happySpecReduce_1  24# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 ((fst happy_var_1, Harper.Abs.BoolLit (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_50 = happySpecReduce_2  25# happyReduction_50
happyReduction_50 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut4 happy_x_2 of { happy_var_2 -> 
	happyIn29
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.MembAcc (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_51 = happySpecReduce_1  26# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_52 = happySpecReduce_3  26# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn30
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_53 = happySpecReduce_1  27# happyReduction_53
happyReduction_53 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.ThisExpr (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_54 = happySpecReduce_3  27# happyReduction_54
happyReduction_54 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.TupExpr (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_55 = happySpecReduce_1  27# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((fst happy_var_1, Harper.Abs.LitExpr (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_56 = happySpecReduce_1  27# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((fst happy_var_1, Harper.Abs.ObjExpr (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_57 = happySpecReduce_3  27# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), snd happy_var_2)
	)}}

happyReduce_58 = happySpecReduce_2  28# happyReduction_58
happyReduction_58 happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn32
		 ((fst happy_var_1, Harper.Abs.TMembExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_59 = happySpecReduce_2  28# happyReduction_59
happyReduction_59 happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn32
		 ((fst happy_var_1, Harper.Abs.MembExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_60 = happySpecReduce_2  28# happyReduction_60
happyReduction_60 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn32
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.DataExpr (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_61 = happySpecReduce_1  28# happyReduction_61
happyReduction_61 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_62 = happyReduce 4# 29# happyReduction_62
happyReduction_62 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.AdHocExpr (Just (tokenLineCol happy_var_1)) (reverse (snd happy_var_3)))
	) `HappyStk` happyRest}}

happyReduce_63 = happyReduce 5# 29# happyReduction_63
happyReduction_63 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_2 of { happy_var_2 -> 
	case happyOut49 happy_x_4 of { happy_var_4 -> 
	happyIn33
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.VCtorExpr (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_64 = happySpecReduce_1  29# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_65 = happyReduce 5# 30# happyReduction_65
happyReduction_65 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	case happyOut47 happy_x_4 of { happy_var_4 -> 
	happyIn34
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.MatchExpr (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_66 = happySpecReduce_1  30# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_67 = happySpecReduce_2  31# happyReduction_67
happyReduction_67 happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	happyIn35
		 ((fst happy_var_1, Harper.Abs.AppExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_68 = happySpecReduce_1  31# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_69 = happySpecReduce_3  32# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 ((fst happy_var_1, Harper.Abs.CompExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_70 = happySpecReduce_1  32# happyReduction_70
happyReduction_70 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_71 = happySpecReduce_3  33# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 ((fst happy_var_1, Harper.Abs.PowExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_72 = happySpecReduce_1  33# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_73 = happySpecReduce_3  34# happyReduction_73
happyReduction_73 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 ((fst happy_var_1, Harper.Abs.MulExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_74 = happySpecReduce_3  34# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 ((fst happy_var_1, Harper.Abs.DivExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_75 = happySpecReduce_3  34# happyReduction_75
happyReduction_75 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 ((fst happy_var_1, Harper.Abs.ModExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_76 = happySpecReduce_1  34# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_77 = happySpecReduce_3  35# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 ((fst happy_var_1, Harper.Abs.AddExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_78 = happySpecReduce_3  35# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 ((fst happy_var_1, Harper.Abs.SubExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_79 = happySpecReduce_1  35# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_80 = happySpecReduce_2  36# happyReduction_80
happyReduction_80 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn40
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.NotExpr (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_81 = happySpecReduce_2  36# happyReduction_81
happyReduction_81 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn40
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.NegExpr (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_82 = happySpecReduce_1  36# happyReduction_82
happyReduction_82 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_83 = happySpecReduce_3  37# happyReduction_83
happyReduction_83 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 ((fst happy_var_1, Harper.Abs.EqExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_84 = happySpecReduce_3  37# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 ((fst happy_var_1, Harper.Abs.NEqExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_85 = happySpecReduce_3  37# happyReduction_85
happyReduction_85 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 ((fst happy_var_1, Harper.Abs.LtExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_86 = happySpecReduce_3  37# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 ((fst happy_var_1, Harper.Abs.GtExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_87 = happySpecReduce_3  37# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 ((fst happy_var_1, Harper.Abs.LEqExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_88 = happySpecReduce_3  37# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn41
		 ((fst happy_var_1, Harper.Abs.GEqExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_89 = happySpecReduce_1  37# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_90 = happySpecReduce_3  38# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut41 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 ((fst happy_var_1, Harper.Abs.AndExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_91 = happySpecReduce_3  38# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut41 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 ((fst happy_var_1, Harper.Abs.OrExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_92 = happySpecReduce_1  38# happyReduction_92
happyReduction_92 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_93 = happySpecReduce_3  39# happyReduction_93
happyReduction_93 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 ((fst happy_var_1, Harper.Abs.SeqExpr (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_94 = happySpecReduce_1  39# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_95 = happyReduce 4# 40# happyReduction_95
happyReduction_95 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_4 of { happy_var_4 -> 
	happyIn44
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.LamExpr (Just (tokenLineCol happy_var_1)) (reverse (snd happy_var_2)) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_96 = happySpecReduce_1  40# happyReduction_96
happyReduction_96 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_97 = happySpecReduce_3  41# happyReduction_97
happyReduction_97 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 ((fst happy_var_1, Harper.Abs.TupExprList (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_98 = happySpecReduce_3  41# happyReduction_98
happyReduction_98 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 ((fst happy_var_1, Harper.Abs.TupExprTail (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_99 = happySpecReduce_3  42# happyReduction_99
happyReduction_99 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut66 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn46
		 ((fst happy_var_1, Harper.Abs.MatchExprClause (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_100 = happySpecReduce_1  43# happyReduction_100
happyReduction_100 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_101 = happySpecReduce_3  43# happyReduction_101
happyReduction_101 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_102 = happySpecReduce_3  44# happyReduction_102
happyReduction_102 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn48
		 ((fst happy_var_1, Harper.Abs.DataAss (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_103 = happySpecReduce_0  45# happyReduction_103
happyReduction_103  =  happyIn49
		 ((Nothing, [])
	)

happyReduce_104 = happySpecReduce_1  45# happyReduction_104
happyReduction_104 happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	happyIn49
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_105 = happySpecReduce_3  45# happyReduction_105
happyReduction_105 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	case happyOut49 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_106 = happySpecReduce_2  46# happyReduction_106
happyReduction_106 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn50
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.EmptyStmt (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_107 = happySpecReduce_3  46# happyReduction_107
happyReduction_107 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { happy_var_2 -> 
	happyIn50
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.StmtBlock (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_108 = happyReduce 5# 46# happyReduction_108
happyReduction_108 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { happy_var_2 -> 
	case happyOut76 happy_x_4 of { happy_var_4 -> 
	happyIn50
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.StmtBlockWDecls (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_109 = happySpecReduce_2  47# happyReduction_109
happyReduction_109 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn51
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.RetStmt (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_110 = happySpecReduce_3  47# happyReduction_110
happyReduction_110 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_2 of { happy_var_2 -> 
	happyIn51
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.RetExprStmt (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_111 = happySpecReduce_2  47# happyReduction_111
happyReduction_111 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn51
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.CntStmt (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_112 = happySpecReduce_2  47# happyReduction_112
happyReduction_112 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn51
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.BrkStmt (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_113 = happySpecReduce_3  47# happyReduction_113
happyReduction_113 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_2 of { happy_var_2 -> 
	happyIn51
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.YieldStmt (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_114 = happySpecReduce_1  47# happyReduction_114
happyReduction_114 happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	happyIn51
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_115 = happyReduce 5# 48# happyReduction_115
happyReduction_115 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_2 of { happy_var_2 -> 
	case happyOut58 happy_x_4 of { happy_var_4 -> 
	happyIn52
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.MatchStmt (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_116 = happySpecReduce_3  48# happyReduction_116
happyReduction_116 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_2 of { happy_var_2 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn52
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.WhileStmt (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_3))
	)}}}

happyReduce_117 = happyReduce 5# 48# happyReduction_117
happyReduction_117 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut66 happy_x_2 of { happy_var_2 -> 
	case happyOut44 happy_x_4 of { happy_var_4 -> 
	case happyOut50 happy_x_5 of { happy_var_5 -> 
	happyIn52
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.ForInStmt (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_4)(snd happy_var_5))
	) `HappyStk` happyRest}}}}

happyReduce_118 = happySpecReduce_1  48# happyReduction_118
happyReduction_118 happy_x_1
	 =  case happyOut59 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 ((fst happy_var_1, Harper.Abs.CondStmt (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_119 = happySpecReduce_1  48# happyReduction_119
happyReduction_119 happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_120 = happyReduce 4# 49# happyReduction_120
happyReduction_120 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut66 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 ((fst happy_var_1, Harper.Abs.DconStmt (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_121 = happySpecReduce_2  49# happyReduction_121
happyReduction_121 happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_1 of { happy_var_1 -> 
	happyIn53
		 ((fst happy_var_1, Harper.Abs.DeclStmt (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_122 = happySpecReduce_1  49# happyReduction_122
happyReduction_122 happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	happyIn53
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_123 = happyReduce 4# 50# happyReduction_123
happyReduction_123 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 ((fst happy_var_1, Harper.Abs.AssStmt (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_124 = happyReduce 4# 50# happyReduction_124
happyReduction_124 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 ((fst happy_var_1, Harper.Abs.AddStmt (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_125 = happyReduce 4# 50# happyReduction_125
happyReduction_125 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 ((fst happy_var_1, Harper.Abs.SubStmt (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_126 = happyReduce 4# 50# happyReduction_126
happyReduction_126 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 ((fst happy_var_1, Harper.Abs.MulStmt (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_127 = happyReduce 4# 50# happyReduction_127
happyReduction_127 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 ((fst happy_var_1, Harper.Abs.DivStmt (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_128 = happyReduce 4# 50# happyReduction_128
happyReduction_128 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 ((fst happy_var_1, Harper.Abs.PowStmt (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_129 = happyReduce 4# 50# happyReduction_129
happyReduction_129 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn54
		 ((fst happy_var_1, Harper.Abs.CompStmt (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_130 = happySpecReduce_1  50# happyReduction_130
happyReduction_130 happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	happyIn54
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_131 = happySpecReduce_3  51# happyReduction_131
happyReduction_131 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn55
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.EvalStmt (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_132 = happySpecReduce_1  51# happyReduction_132
happyReduction_132 happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	happyIn55
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_133 = happySpecReduce_1  52# happyReduction_133
happyReduction_133 happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_134 = happySpecReduce_2  52# happyReduction_134
happyReduction_134 happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { happy_var_2 -> 
	happyIn56
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_135 = happySpecReduce_3  53# happyReduction_135
happyReduction_135 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut66 happy_x_1 of { happy_var_1 -> 
	case happyOut51 happy_x_3 of { happy_var_3 -> 
	happyIn57
		 ((fst happy_var_1, Harper.Abs.MatchStmtClause (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_136 = happySpecReduce_1  54# happyReduction_136
happyReduction_136 happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	happyIn58
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_137 = happySpecReduce_2  54# happyReduction_137
happyReduction_137 happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn58
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_138 = happySpecReduce_2  55# happyReduction_138
happyReduction_138 happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	case happyOut63 happy_x_2 of { happy_var_2 -> 
	happyIn59
		 ((fst happy_var_1, Harper.Abs.IfElifStmts (fst happy_var_1)(snd happy_var_1)(reverse (snd happy_var_2)))
	)}}

happyReduce_139 = happySpecReduce_3  55# happyReduction_139
happyReduction_139 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_1 of { happy_var_1 -> 
	case happyOut63 happy_x_2 of { happy_var_2 -> 
	case happyOut62 happy_x_3 of { happy_var_3 -> 
	happyIn59
		 ((fst happy_var_1, Harper.Abs.IfElifElseStmts (fst happy_var_1)(snd happy_var_1)(reverse (snd happy_var_2)) (snd happy_var_3))
	)}}}

happyReduce_140 = happySpecReduce_3  56# happyReduction_140
happyReduction_140 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_2 of { happy_var_2 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	happyIn60
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.IfStmt (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_3))
	)}}}

happyReduce_141 = happyReduce 4# 57# happyReduction_141
happyReduction_141 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	case happyOut50 happy_x_4 of { happy_var_4 -> 
	happyIn61
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.ElifStmt (Just (tokenLineCol happy_var_1)) (snd happy_var_3)(snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_142 = happySpecReduce_2  58# happyReduction_142
happyReduction_142 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_2 of { happy_var_2 -> 
	happyIn62
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.ElseStmt (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_143 = happySpecReduce_0  59# happyReduction_143
happyReduction_143  =  happyIn63
		 ((Nothing, [])
	)

happyReduce_144 = happySpecReduce_2  59# happyReduction_144
happyReduction_144 happy_x_2
	happy_x_1
	 =  case happyOut63 happy_x_1 of { happy_var_1 -> 
	case happyOut61 happy_x_2 of { happy_var_2 -> 
	happyIn63
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_145 = happySpecReduce_1  60# happyReduction_145
happyReduction_145 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn64
		 ((fst happy_var_1, Harper.Abs.PatLit (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_146 = happySpecReduce_1  61# happyReduction_146
happyReduction_146 happy_x_1
	 =  case happyOut64 happy_x_1 of { happy_var_1 -> 
	happyIn65
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_147 = happySpecReduce_1  61# happyReduction_147
happyReduction_147 happy_x_1
	 =  case happyOut75 happy_x_1 of { happy_var_1 -> 
	happyIn65
		 ((fst happy_var_1, Harper.Abs.PatDecl (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_148 = happyReduce 4# 61# happyReduction_148
happyReduction_148 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut69 happy_x_3 of { happy_var_3 -> 
	happyIn65
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.PatData (Just (tokenLineCol happy_var_1)) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_149 = happySpecReduce_3  61# happyReduction_149
happyReduction_149 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut67 happy_x_2 of { happy_var_2 -> 
	happyIn65
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.PatTup (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_150 = happySpecReduce_1  61# happyReduction_150
happyReduction_150 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn65
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.PatDisc (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_151 = happySpecReduce_1  62# happyReduction_151
happyReduction_151 happy_x_1
	 =  case happyOut65 happy_x_1 of { happy_var_1 -> 
	happyIn66
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_152 = happyReduce 4# 62# happyReduction_152
happyReduction_152 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut69 happy_x_3 of { happy_var_3 -> 
	happyIn66
		 ((fst happy_var_1, Harper.Abs.PatCtor (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_153 = happySpecReduce_3  63# happyReduction_153
happyReduction_153 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut66 happy_x_1 of { happy_var_1 -> 
	case happyOut67 happy_x_3 of { happy_var_3 -> 
	happyIn67
		 ((fst happy_var_1, Harper.Abs.PatTupList (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_154 = happySpecReduce_3  63# happyReduction_154
happyReduction_154 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut66 happy_x_1 of { happy_var_1 -> 
	case happyOut66 happy_x_3 of { happy_var_3 -> 
	happyIn67
		 ((fst happy_var_1, Harper.Abs.PatTupTail (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_155 = happySpecReduce_3  64# happyReduction_155
happyReduction_155 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	case happyOut66 happy_x_3 of { happy_var_3 -> 
	happyIn68
		 ((fst happy_var_1, Harper.Abs.PatFld (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_156 = happySpecReduce_0  65# happyReduction_156
happyReduction_156  =  happyIn69
		 ((Nothing, [])
	)

happyReduce_157 = happySpecReduce_1  65# happyReduction_157
happyReduction_157 happy_x_1
	 =  case happyOut68 happy_x_1 of { happy_var_1 -> 
	happyIn69
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_158 = happySpecReduce_3  65# happyReduction_158
happyReduction_158 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut68 happy_x_1 of { happy_var_1 -> 
	case happyOut69 happy_x_3 of { happy_var_3 -> 
	happyIn69
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_159 = happySpecReduce_1  66# happyReduction_159
happyReduction_159 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn70
		 ((fst happy_var_1, Harper.Abs.Decl (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_160 = happySpecReduce_3  66# happyReduction_160
happyReduction_160 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn70
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.DeclWHint (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_161 = happySpecReduce_3  67# happyReduction_161
happyReduction_161 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut70 happy_x_1 of { happy_var_1 -> 
	case happyOut44 happy_x_3 of { happy_var_3 -> 
	happyIn71
		 ((fst happy_var_1, Harper.Abs.AdHocFld (fst happy_var_1)(snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_162 = happySpecReduce_0  68# happyReduction_162
happyReduction_162  =  happyIn72
		 ((Nothing, [])
	)

happyReduce_163 = happySpecReduce_3  68# happyReduction_163
happyReduction_163 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	case happyOut71 happy_x_2 of { happy_var_2 -> 
	happyIn72
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_164 = happySpecReduce_1  69# happyReduction_164
happyReduction_164 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn73
		 ((fst happy_var_1, Harper.Abs.LocTHint (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_165 = happySpecReduce_1  69# happyReduction_165
happyReduction_165 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn73
		 ((fst happy_var_1, Harper.Abs.LocFDecl (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_166 = happySpecReduce_2  70# happyReduction_166
happyReduction_166 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut70 happy_x_2 of { happy_var_2 -> 
	happyIn74
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.LocVarDecl (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_167 = happySpecReduce_1  71# happyReduction_167
happyReduction_167 happy_x_1
	 =  case happyOut74 happy_x_1 of { happy_var_1 -> 
	happyIn75
		 ((fst happy_var_1, snd happy_var_1)
	)}

happyReduce_168 = happySpecReduce_1  71# happyReduction_168
happyReduction_168 happy_x_1
	 =  case happyOut70 happy_x_1 of { happy_var_1 -> 
	happyIn75
		 ((fst happy_var_1, Harper.Abs.LocValDecl (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_169 = happySpecReduce_2  72# happyReduction_169
happyReduction_169 happy_x_2
	happy_x_1
	 =  case happyOut73 happy_x_1 of { happy_var_1 -> 
	happyIn76
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_170 = happySpecReduce_3  72# happyReduction_170
happyReduction_170 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut73 happy_x_1 of { happy_var_1 -> 
	case happyOut76 happy_x_3 of { happy_var_3 -> 
	happyIn76
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_171 = happySpecReduce_2  73# happyReduction_171
happyReduction_171 happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut80 happy_x_2 of { happy_var_2 -> 
	happyIn77
		 ((fst happy_var_1, Harper.Abs.TSig (fst happy_var_1)(snd happy_var_1)(reverse (snd happy_var_2)))
	)}}

happyReduce_172 = happyReduce 6# 74# happyReduction_172
happyReduction_172 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut77 happy_x_2 of { happy_var_2 -> 
	case happyOut83 happy_x_5 of { happy_var_5 -> 
	happyIn78
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.ValTDecl (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_173 = happyReduce 6# 74# happyReduction_173
happyReduction_173 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut77 happy_x_2 of { happy_var_2 -> 
	case happyOut82 happy_x_5 of { happy_var_5 -> 
	happyIn78
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.ValTUDecl (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_174 = happyReduce 6# 74# happyReduction_174
happyReduction_174 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut77 happy_x_2 of { happy_var_2 -> 
	case happyOut83 happy_x_5 of { happy_var_5 -> 
	happyIn78
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.RefTDecl (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_175 = happySpecReduce_1  75# happyReduction_175
happyReduction_175 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn79
		 ((fst happy_var_1, Harper.Abs.TParam (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_176 = happySpecReduce_0  76# happyReduction_176
happyReduction_176  =  happyIn80
		 ((Nothing, [])
	)

happyReduce_177 = happySpecReduce_2  76# happyReduction_177
happyReduction_177 happy_x_2
	happy_x_1
	 =  case happyOut80 happy_x_1 of { happy_var_1 -> 
	case happyOut79 happy_x_2 of { happy_var_2 -> 
	happyIn80
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyReduce_178 = happyReduce 6# 77# happyReduction_178
happyReduction_178 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_2 of { happy_var_2 -> 
	case happyOut83 happy_x_5 of { happy_var_5 -> 
	happyIn81
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.TVarDecl (Just (tokenLineCol happy_var_1)) (snd happy_var_2)(snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_179 = happySpecReduce_2  78# happyReduction_179
happyReduction_179 happy_x_2
	happy_x_1
	 =  case happyOut81 happy_x_1 of { happy_var_1 -> 
	happyIn82
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_180 = happySpecReduce_3  78# happyReduction_180
happyReduction_180 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut81 happy_x_1 of { happy_var_1 -> 
	case happyOut82 happy_x_3 of { happy_var_3 -> 
	happyIn82
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_181 = happyReduce 6# 79# happyReduction_181
happyReduction_181 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut85 happy_x_4 of { happy_var_4 -> 
	case happyOut87 happy_x_6 of { happy_var_6 -> 
	happyIn83
		 ((Just (tokenLineCol happy_var_1), Harper.Abs.DataTBody (Just (tokenLineCol happy_var_1)) (snd happy_var_4)(reverse (snd happy_var_6)))
	) `HappyStk` happyRest}}}

happyReduce_182 = happySpecReduce_1  79# happyReduction_182
happyReduction_182 happy_x_1
	 =  case happyOut87 happy_x_1 of { happy_var_1 -> 
	happyIn83
		 ((fst happy_var_1, Harper.Abs.TBody (fst happy_var_1)(reverse (snd happy_var_1)))
	)}

happyReduce_183 = happySpecReduce_1  80# happyReduction_183
happyReduction_183 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn84
		 ((fst happy_var_1, Harper.Abs.TFldDecl (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_184 = happySpecReduce_2  81# happyReduction_184
happyReduction_184 happy_x_2
	happy_x_1
	 =  case happyOut84 happy_x_1 of { happy_var_1 -> 
	happyIn85
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_185 = happySpecReduce_3  81# happyReduction_185
happyReduction_185 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut84 happy_x_1 of { happy_var_1 -> 
	case happyOut85 happy_x_3 of { happy_var_3 -> 
	happyIn85
		 ((fst happy_var_1, (:) (snd happy_var_1)(snd happy_var_3))
	)}}

happyReduce_186 = happySpecReduce_1  82# happyReduction_186
happyReduction_186 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn86
		 ((fst happy_var_1, Harper.Abs.TMemTHint (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_187 = happySpecReduce_1  82# happyReduction_187
happyReduction_187 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn86
		 ((fst happy_var_1, Harper.Abs.TMemFDecl (fst happy_var_1)(snd happy_var_1))
	)}

happyReduce_188 = happySpecReduce_0  83# happyReduction_188
happyReduction_188  =  happyIn87
		 ((Nothing, [])
	)

happyReduce_189 = happySpecReduce_3  83# happyReduction_189
happyReduction_189 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut87 happy_x_1 of { happy_var_1 -> 
	case happyOut86 happy_x_2 of { happy_var_2 -> 
	happyIn87
		 ((fst happy_var_1, flip (:) (snd happy_var_1)(snd happy_var_2))
	)}}

happyNewToken action sts stk [] =
	happyDoAction 69# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TS _ 41) -> cont 41#;
	PT _ (TS _ 42) -> cont 42#;
	PT _ (TS _ 43) -> cont 43#;
	PT _ (TS _ 44) -> cont 44#;
	PT _ (TS _ 45) -> cont 45#;
	PT _ (TS _ 46) -> cont 46#;
	PT _ (TS _ 47) -> cont 47#;
	PT _ (TS _ 48) -> cont 48#;
	PT _ (TS _ 49) -> cont 49#;
	PT _ (TS _ 50) -> cont 50#;
	PT _ (TS _ 51) -> cont 51#;
	PT _ (TS _ 52) -> cont 52#;
	PT _ (TS _ 53) -> cont 53#;
	PT _ (TS _ 54) -> cont 54#;
	PT _ (TS _ 55) -> cont 55#;
	PT _ (TS _ 56) -> cont 56#;
	PT _ (TS _ 57) -> cont 57#;
	PT _ (TS _ 58) -> cont 58#;
	PT _ (TS _ 59) -> cont 59#;
	PT _ (TS _ 60) -> cont 60#;
	PT _ (TS _ 61) -> cont 61#;
	PT _ (TS _ 62) -> cont 62#;
	PT _ (TS _ 63) -> cont 63#;
	PT _ (TV _) -> cont 64#;
	PT _ (TI _) -> cont 65#;
	PT _ (TC _) -> cont 66#;
	PT _ (TL _) -> cont 67#;
	PT _ (T_UIdent _) -> cont 68#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 69# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut9 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens

pProgram = (>>= return . snd) . pProgram_internal
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 11 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/tmp/ghcb5f8_0/ghc_2.h" #-}




























































































































































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+#  i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 180 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+#  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
