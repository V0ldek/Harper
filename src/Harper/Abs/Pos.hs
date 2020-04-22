{-# LANGUAGE FlexibleInstances #-}
module Harper.Abs.Pos where

import           Harper.Abs

class Position a where
    pos :: a -> Pos

instance Position (Program Pos) where
    pos (Prog a _) = a

instance Position (TopLvlDecl Pos) where
    pos (TopLvlFDecl a _) = a
    pos (TopLvlTHint a _) = a
    pos (TopLvlTDecl a _) = a

instance Position (TypeHint Pos) where
    pos (THint a _ _) = a

instance Position (TypeExpr Pos) where
    pos (TVar  a _ ) = a
    pos (TCtor a _ ) = a
    pos (TPur  a _ ) = a
    pos (TUnit a   ) = a
    pos (TTup   a _) = a
    pos (TAdHoc a _) = a
    pos (TApp a _ _) = a
    pos (TFun a _ _) = a

instance Position (TupleType Pos) where
    pos (TTupList a _ _) = a
    pos (TTupTail a _ _) = a

instance Position (TypePurity Pos) where
    pos (TImpure a) = a
    pos (TSideE  a) = a

instance Position (FieldTypeExpr Pos) where
    pos (TFld a _) = a

instance Position (FunDecl Pos) where
    pos (FDecl a _ _ _) = a

instance Position (FunArg Pos) where
    pos (FArg a _) = a

instance Position (LambdaArg Pos) where
    pos (LamArg a _) = a

instance Position (FunBody Pos) where
    pos (FExprBody a _) = a
    pos (FStmtBody a _) = a

instance Position (BoolLiteral Pos) where
    pos (BTrue  a) = a
    pos (BFalse a) = a

instance Position (Literal Pos) where
    pos (UnitLit a  ) = a
    pos (IntLit  a _) = a
    pos (CharLit a _) = a
    pos (StrLit  a _) = a
    pos (BoolLit a _) = a

instance Position (Qualifier Pos) where
    pos (Qual a _   ) = a
    pos (Quals a _ _) = a
    pos (ThisQual a ) = a
    pos (DataQual a ) = a

instance Position (Expression Pos) where
    pos (ThisExpr a     ) = a
    pos (AdHocExpr a _  ) = a
    pos (VCtorExpr a _ _) = a
    pos (TupExpr  a _   ) = a
    pos (LitExpr  a _   ) = a
    pos (ObjExpr  a _   ) = a
    pos (CtorExpr a _   ) = a
    pos (QObjExpr  a _ _) = a
    pos (MatchExpr a _ _) = a
    pos (AppExpr   a _ _) = a
    pos (CompExpr  a _ _) = a
    pos (PowExpr   a _ _) = a
    pos (MulExpr   a _ _) = a
    pos (DivExpr   a _ _) = a
    pos (ModExpr   a _ _) = a
    pos (AddExpr   a _ _) = a
    pos (SubExpr   a _ _) = a
    pos (NotExpr a _    ) = a
    pos (NegExpr a _    ) = a
    pos (EqExpr  a _ _  ) = a
    pos (NEqExpr a _ _  ) = a
    pos (LtExpr  a _ _  ) = a
    pos (GtExpr  a _ _  ) = a
    pos (LEqExpr a _ _  ) = a
    pos (GEqExpr a _ _  ) = a
    pos (AndExpr a _ _  ) = a
    pos (OrExpr  a _ _  ) = a
    pos (SeqExpr a _ _  ) = a
    pos (LamExpr a _ _  ) = a

instance Position (TupleExpression Pos) where
    pos (TupExprList a _ _) = a
    pos (TupExprTail a _ _) = a

instance Position (MatchExpressionClause Pos) where
    pos (MatchExprClause a _ _) = a

instance Position (FieldAss Pos) where
    pos (DataAss a _ _) = a

instance Position (Statement Pos) where
    pos (EmptyStmt a          ) = a
    pos (StmtBlock a _        ) = a
    pos (StmtBlockWDecls a _ _) = a
    pos (RetStmt a            ) = a
    pos (RetExprStmt a _      ) = a
    pos (CntStmt a            ) = a
    pos (BrkStmt a            ) = a
    pos (YieldStmt a _        ) = a
    pos (MatchStmt a _ _      ) = a
    pos (WhileStmt a _ _      ) = a
    pos (ForInStmt a _ _ _    ) = a
    pos (CondStmt a _         ) = a
    pos (DconStmt a _ _       ) = a
    pos (DeclStmt a _         ) = a
    pos (AssStmt  a _ _       ) = a
    pos (AddStmt  a _ _       ) = a
    pos (SubStmt  a _ _       ) = a
    pos (MulStmt  a _ _       ) = a
    pos (DivStmt  a _ _       ) = a
    pos (PowStmt  a _ _       ) = a
    pos (CompStmt a _ _       ) = a
    pos (QAssStmt  a _ _ _    ) = a
    pos (QAddStmt  a _ _ _    ) = a
    pos (QSubStmt  a _ _ _    ) = a
    pos (QMulStmt  a _ _ _    ) = a
    pos (QDivStmt  a _ _ _    ) = a
    pos (QPowStmt  a _ _ _    ) = a
    pos (QCompStmt a _ _ _    ) = a
    pos (EvalStmt a _         ) = a

instance Position (MatchStatementClause Pos) where
    pos (MatchStmtClause a _ _) = a

instance Position (ConditionalStatement Pos) where
    pos (IfElifStmts a _ _      ) = a
    pos (IfElifElseStmts a _ _ _) = a

instance Position (IfStatement Pos) where
    pos (IfStmt a _ _) = a

instance Position (ElseIfStatement Pos) where
    pos (ElifStmt a _ _) = a

instance Position (ElseStatement Pos) where
    pos (ElseStmt a _) = a

instance Position (Pattern Pos) where
    pos (PatLit  a _  ) = a
    pos (PatDecl a _  ) = a
    pos (PatData a _  ) = a
    pos (PatTup  a _  ) = a
    pos (PatDisc a    ) = a
    pos (PatCtor a _ _) = a

instance Position (TuplePattern Pos) where
    pos (PatTupList a _ _) = a
    pos (PatTupTail a _ _) = a

instance Position (FieldPattern Pos) where
    pos (PatFld a _ _) = a

instance Position (Declaration Pos) where
    pos (Decl      a _) = a
    pos (DeclWHint a _) = a

instance Position (AdHocFieldDecl Pos) where
    pos (AdHocFld a _ _) = a

instance Position (LocalFunDecl Pos) where
    pos (LocTHint a _) = a
    pos (LocFDecl a _) = a

instance Position (LocalObjDecl Pos) where
    pos (LocVarDecl a _) = a
    pos (LocValDecl a _) = a

instance Position (TypeSignature Pos) where
    pos (TSig a _ _) = a

instance Position (TypeDecl Pos) where
    pos (ValTDecl  a _ _) = a
    pos (RefTDecl   a _ _) = a
    pos (ValTUDecl a _ _) = a

instance Position (TypeArgument Pos) where
    pos (TArg a _) = a

instance Position (TypeVariantDecl Pos) where
    pos (TVarDecl a _ _) = a

instance Position (TypeBody Pos) where
    pos (DataTBody a _ _) = a
    pos (TBody a _      ) = a

instance Position (FieldDecl Pos) where
    pos (TFldDecl a _) = a

instance Position (MemberDecl Pos) where
    pos (TMemTHint a _) = a
    pos (TMemFDecl a _) = a

