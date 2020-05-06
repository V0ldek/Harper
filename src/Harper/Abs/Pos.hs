{-# LANGUAGE FlexibleInstances #-}
module Harper.Abs.Pos where

import           Harper.Abs

type Pos = Maybe (Int, Int)

class Position a where
    pos :: a -> Pos

instance Position Pos where
    pos = id

instance (Position b) => Position (a, b) where
    pos (_, b) = pos b

instance (Position a) => Position (Program a) where
    pos (Prog a _) = pos a

instance (Position a) => Position (TopLvlDecl a) where
    pos (TopLvlFDecl a _) = pos a
    pos (TopLvlTHint a _) = pos a
    pos (TopLvlTDecl a _) = pos a

instance (Position a) => Position (TypeHint a) where
    pos (THint a _ _) = pos a

instance (Position a) => Position (TypeExpr a) where
    pos (TVar  a _ ) = pos a
    pos (TCtor a _ ) = pos a
    pos (TPur  a _ ) = pos a
    pos (TUnit a   ) = pos a
    pos (TTup   a _) = pos a
    pos (TAdHoc a _) = pos a
    pos (TApp a _ _) = pos a
    pos (TFun a _ _) = pos a

instance (Position a) => Position (TupleType a) where
    pos (TTupList a _ _) = pos a
    pos (TTupTail a _ _) = pos a

instance (Position a) => Position (TypePurity a) where
    pos (TImpure a) = pos a
    pos (TSideE  a) = pos a

instance (Position a) => Position (FieldTypeExpr a) where
    pos (TFld a _) = pos a

instance (Position a) => Position (FunDecl a) where
    pos (FDecl a _ _ _) = pos a

instance (Position a) => Position (FunParam a) where
    pos (FParam a _) = pos a

instance (Position a) => Position (LambdaParam a) where
    pos (LamParam a _) = pos a

instance (Position a) => Position (FunBody a) where
    pos (FExprBody  a _) = pos a
    pos (FStmtBody  a _) = pos a
    pos (FVIterBody a _) = pos a
    pos (FRIterBody a _) = pos a

instance (Position a) => Position (BoolLiteral a) where
    pos (BTrue  a) = pos a
    pos (BFalse a) = pos a

instance (Position a) => Position (Literal a) where
    pos (UnitLit a  ) = pos a
    pos (IntLit  a _) = pos a
    pos (CharLit a _) = pos a
    pos (StrLit  a _) = pos a
    pos (BoolLit a _) = pos a

instance (Position a) => Position (MemberAccess a) where
    pos (MembAcc a _) = pos a

instance (Position a) => Position (Expression a) where
    pos (ThisExpr a     ) = pos a
    pos (AdHocExpr a _  ) = pos a
    pos (VCtorExpr a _ _) = pos a
    pos (TupExpr a _    ) = pos a
    pos (LitExpr a _    ) = pos a
    pos (ObjExpr a _    ) = pos a
    pos (TMembExpr a _ _) = pos a
    pos (MembExpr  a _ _) = pos a
    pos (DataExpr a _   ) = pos a
    pos (MatchExpr a _ _) = pos a
    pos (AppExpr   a _ _) = pos a
    pos (CompExpr  a _ _) = pos a
    pos (PowExpr   a _ _) = pos a
    pos (MulExpr   a _ _) = pos a
    pos (DivExpr   a _ _) = pos a
    pos (ModExpr   a _ _) = pos a
    pos (AddExpr   a _ _) = pos a
    pos (SubExpr   a _ _) = pos a
    pos (NotExpr a _    ) = pos a
    pos (NegExpr a _    ) = pos a
    pos (EqExpr  a _ _  ) = pos a
    pos (NEqExpr a _ _  ) = pos a
    pos (LtExpr  a _ _  ) = pos a
    pos (GtExpr  a _ _  ) = pos a
    pos (LEqExpr a _ _  ) = pos a
    pos (GEqExpr a _ _  ) = pos a
    pos (AndExpr a _ _  ) = pos a
    pos (OrExpr  a _ _  ) = pos a
    pos (SeqExpr a _ _  ) = pos a
    pos (LamExpr a _ _  ) = pos a

instance (Position a) => Position (TupleExpression a) where
    pos (TupExprList a _ _) = pos a
    pos (TupExprTail a _ _) = pos a

instance (Position a) => Position (MatchExpressionClause a) where
    pos (MatchExprClause a _ _) = pos a

instance (Position a) => Position (FieldAss a) where
    pos (DataAss a _ _) = pos a

instance (Position a) => Position (Statement a) where
    pos (EmptyStmt a          ) = pos a
    pos (StmtBlock a _        ) = pos a
    pos (StmtBlockWDecls a _ _) = pos a
    pos (RetStmt a            ) = pos a
    pos (RetExprStmt a _      ) = pos a
    pos (CntStmt a            ) = pos a
    pos (BrkStmt a            ) = pos a
    pos (YieldStmt a _        ) = pos a
    pos (YieldRetStmt a       ) = pos a
    pos (MatchStmt a _ _      ) = pos a
    pos (WhileStmt a _ _      ) = pos a
    pos (ForInStmt  a _ _ _   ) = pos a
    pos (ForInVStmt a _ _ _   ) = pos a
    pos (ForInRStmt a _ _ _   ) = pos a
    pos (CondStmt a _         ) = pos a
    pos (DconStmt a _ _       ) = pos a
    pos (DeclStmt a _         ) = pos a
    pos (AssStmt      a _ _   ) = pos a
    pos (AddStmt      a _ _   ) = pos a
    pos (SubStmt      a _ _   ) = pos a
    pos (MulStmt      a _ _   ) = pos a
    pos (DivStmt      a _ _   ) = pos a
    pos (PowStmt      a _ _   ) = pos a
    pos (CompStmt     a _ _   ) = pos a
    pos (DataAssStmt  a _ _   ) = pos a
    pos (DataAddStmt  a _ _   ) = pos a
    pos (DataSubStmt  a _ _   ) = pos a
    pos (DataMulStmt  a _ _   ) = pos a
    pos (DataDivStmt  a _ _   ) = pos a
    pos (DataPowStmt  a _ _   ) = pos a
    pos (DataCompStmt a _ _   ) = pos a
    pos (EvalStmt a _         ) = pos a

instance (Position a) => Position (MatchStatementClause a) where
    pos (MatchStmtClause a _ _) = pos a

instance (Position a) => Position (ConditionalStatement a) where
    pos (IfElifStmts a _ _      ) = pos a
    pos (IfElifElseStmts a _ _ _) = pos a

instance (Position a) => Position (IfStatement a) where
    pos (IfStmt a _ _) = pos a

instance (Position a) => Position (ElseIfStatement a) where
    pos (ElifStmt a _ _) = pos a

instance (Position a) => Position (ElseStatement a) where
    pos (ElseStmt a _) = pos a

instance (Position a) => Position (Pattern a) where
    pos (PatLit  a _  ) = pos a
    pos (PatDecl a _  ) = pos a
    pos (PatData a _  ) = pos a
    pos (PatTup  a _  ) = pos a
    pos (PatDisc a    ) = pos a
    pos (PatCtor a _ _) = pos a

instance (Position a) => Position (TuplePattern a) where
    pos (PatTupList a _ _) = pos a
    pos (PatTupTail a _ _) = pos a

instance (Position a) => Position (FieldPattern a) where
    pos (PatFld a _ _) = pos a

instance (Position a) => Position (Declaration a) where
    pos (Decl      a _) = pos a
    pos (DeclWHint a _) = pos a

instance (Position a) => Position (AdHocFieldDecl a) where
    pos (AdHocFld a _ _) = pos a

instance (Position a) => Position (LocalFunDecl a) where
    pos (LocTHint a _) = pos a
    pos (LocFDecl a _) = pos a

instance (Position a) => Position (LocalObjDecl a) where
    pos (LocVarDecl a _) = pos a
    pos (LocValDecl a _) = pos a

instance (Position a) => Position (TypeSignature a) where
    pos (TSig a _ _) = pos a

instance (Position a) => Position (TypeDecl a) where
    pos (ValTDecl  a _ _) = pos a
    pos (RefTDecl  a _ _) = pos a
    pos (ValTUDecl a _ _) = pos a

instance (Position a) => Position (TypeParameter a) where
    pos (TParam a _) = pos a

instance (Position a) => Position (TypeVariantDecl a) where
    pos (TVarDecl a _ _) = pos a

instance (Position a) => Position (TypeBody a) where
    pos (DataTBody a _ _) = pos a
    pos (TBody a _      ) = pos a

instance (Position a) => Position (FieldDecl a) where
    pos (TFldDecl a _) = pos a

instance (Position a) => Position (MemberDecl a) where
    pos (TMemTHint a _) = pos a
    pos (TMemFDecl a _) = pos a
