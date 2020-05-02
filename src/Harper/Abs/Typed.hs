{-# LANGUAGE FlexibleInstances #-}
module Harper.Abs.Typed where

import           Harper.Abs
import           Harper.TypeSystem.Core

class Typed a where
    typ :: a -> Type

instance Typed Type where
    typ = id

instance (Typed a) => Typed (a, b) where
    typ (t, _) = typ t

instance (Typed a) => Typed (Program a) where
    typ (Prog a _) = typ a

instance (Typed a) => Typed (TopLvlDecl a) where
    typ (TopLvlFDecl a _) = typ a
    typ (TopLvlTHint a _) = typ a
    typ (TopLvlTDecl a _) = typ a

instance (Typed a) => Typed (TypeHint a) where
    typ (THint a _ _) = typ a

instance (Typed a) => Typed (TypeExpr a) where
    typ (TVar  a _ ) = typ a
    typ (TCtor a _ ) = typ a
    typ (TPur  a _ ) = typ a
    typ (TUnit a   ) = typ a
    typ (TTup   a _) = typ a
    typ (TAdHoc a _) = typ a
    typ (TApp a _ _) = typ a
    typ (TFun a _ _) = typ a

instance (Typed a) => Typed (TupleType a) where
    typ (TTupList a _ _) = typ a
    typ (TTupTail a _ _) = typ a

instance (Typed a) => Typed (TypePurity a) where
    typ (TImpure a) = typ a
    typ (TSideE  a) = typ a

instance (Typed a) => Typed (FieldTypeExpr a) where
    typ (TFld a _) = typ a

instance (Typed a) => Typed (FunDecl a) where
    typ (FDecl a _ _ _) = typ a

instance (Typed a) => Typed (FunParam a) where
    typ (FParam a _) = typ a

instance (Typed a) => Typed (LambdaParam a) where
    typ (LamParam a _) = typ a

instance (Typed a) => Typed (FunBody a) where
    typ (FExprBody a _) = typ a
    typ (FStmtBody a _) = typ a

instance (Typed a) => Typed (BoolLiteral a) where
    typ (BTrue  a) = typ a
    typ (BFalse a) = typ a

instance (Typed a) => Typed (Literal a) where
    typ (UnitLit a  ) = typ a
    typ (IntLit  a _) = typ a
    typ (CharLit a _) = typ a
    typ (StrLit  a _) = typ a
    typ (BoolLit a _) = typ a

instance (Typed a) => Typed (Expression a) where
    typ (ThisExpr a     ) = typ a
    typ (AdHocExpr a _  ) = typ a
    typ (VCtorExpr a _ _) = typ a
    typ (TupExpr a _    ) = typ a
    typ (LitExpr a _    ) = typ a
    typ (ObjExpr a _    ) = typ a
    typ (TMembExpr a _ _) = typ a
    typ (MembExpr  a _ _) = typ a
    typ (DataExpr a _   ) = typ a
    typ (MatchExpr a _ _) = typ a
    typ (AppExpr   a _ _) = typ a
    typ (CompExpr  a _ _) = typ a
    typ (PowExpr   a _ _) = typ a
    typ (MulExpr   a _ _) = typ a
    typ (DivExpr   a _ _) = typ a
    typ (ModExpr   a _ _) = typ a
    typ (AddExpr   a _ _) = typ a
    typ (SubExpr   a _ _) = typ a
    typ (NotExpr a _    ) = typ a
    typ (NegExpr a _    ) = typ a
    typ (EqExpr  a _ _  ) = typ a
    typ (NEqExpr a _ _  ) = typ a
    typ (LtExpr  a _ _  ) = typ a
    typ (GtExpr  a _ _  ) = typ a
    typ (LEqExpr a _ _  ) = typ a
    typ (GEqExpr a _ _  ) = typ a
    typ (AndExpr a _ _  ) = typ a
    typ (OrExpr  a _ _  ) = typ a
    typ (SeqExpr a _ _  ) = typ a
    typ (LamExpr a _ _  ) = typ a

instance (Typed a) => Typed (TupleExpression a) where
    typ (TupExprList a _ _) = typ a
    typ (TupExprTail a _ _) = typ a

instance (Typed a) => Typed (MatchExpressionClause a) where
    typ (MatchExprClause a _ _) = typ a

instance (Typed a) => Typed (FieldAss a) where
    typ (DataAss a _ _) = typ a

instance (Typed a) => Typed (Statement a) where
    typ (EmptyStmt a          ) = typ a
    typ (StmtBlock a _        ) = typ a
    typ (StmtBlockWDecls a _ _) = typ a
    typ (RetStmt a            ) = typ a
    typ (RetExprStmt a _      ) = typ a
    typ (CntStmt a            ) = typ a
    typ (BrkStmt a            ) = typ a
    typ (YieldStmt a _        ) = typ a
    typ (MatchStmt a _ _      ) = typ a
    typ (WhileStmt a _ _      ) = typ a
    typ (ForInStmt a _ _ _    ) = typ a
    typ (CondStmt a _         ) = typ a
    typ (DconStmt a _ _       ) = typ a
    typ (DeclStmt a _         ) = typ a
    typ (AssStmt  a _ _       ) = typ a
    typ (AddStmt  a _ _       ) = typ a
    typ (SubStmt  a _ _       ) = typ a
    typ (MulStmt  a _ _       ) = typ a
    typ (DivStmt  a _ _       ) = typ a
    typ (PowStmt  a _ _       ) = typ a
    typ (CompStmt a _ _       ) = typ a
    typ (EvalStmt a _         ) = typ a

instance (Typed a) => Typed (MatchStatementClause a) where
    typ (MatchStmtClause a _ _) = typ a

instance (Typed a) => Typed (ConditionalStatement a) where
    typ (IfElifStmts a _ _      ) = typ a
    typ (IfElifElseStmts a _ _ _) = typ a

instance (Typed a) => Typed (IfStatement a) where
    typ (IfStmt a _ _) = typ a

instance (Typed a) => Typed (ElseIfStatement a) where
    typ (ElifStmt a _ _) = typ a

instance (Typed a) => Typed (ElseStatement a) where
    typ (ElseStmt a _) = typ a

instance (Typed a) => Typed (Pattern a) where
    typ (PatLit  a _  ) = typ a
    typ (PatDecl a _  ) = typ a
    typ (PatData a _  ) = typ a
    typ (PatTup  a _  ) = typ a
    typ (PatDisc a    ) = typ a
    typ (PatCtor a _ _) = typ a

instance (Typed a) => Typed (TuplePattern a) where
    typ (PatTupList a _ _) = typ a
    typ (PatTupTail a _ _) = typ a

instance (Typed a) => Typed (FieldPattern a) where
    typ (PatFld a _ _) = typ a

instance (Typed a) => Typed (Declaration a) where
    typ (Decl      a _) = typ a
    typ (DeclWHint a _) = typ a

instance (Typed a) => Typed (AdHocFieldDecl a) where
    typ (AdHocFld a _ _) = typ a

instance (Typed a) => Typed (LocalFunDecl a) where
    typ (LocTHint a _) = typ a
    typ (LocFDecl a _) = typ a

instance (Typed a) => Typed (LocalObjDecl a) where
    typ (LocVarDecl a _) = typ a
    typ (LocValDecl a _) = typ a

instance (Typed a) => Typed (TypeSignature a) where
    typ (TSig a _ _) = typ a

instance (Typed a) => Typed (TypeDecl a) where
    typ (ValTDecl  a _ _) = typ a
    typ (RefTDecl  a _ _) = typ a
    typ (ValTUDecl a _ _) = typ a

instance (Typed a) => Typed (TypeParameter a) where
    typ (TParam a _) = typ a

instance (Typed a) => Typed (TypeVariantDecl a) where
    typ (TVarDecl a _ _) = typ a

instance (Typed a) => Typed (TypeBody a) where
    typ (DataTBody a _ _) = typ a
    typ (TBody a _      ) = typ a

instance (Typed a) => Typed (FieldDecl a) where
    typ (TFldDecl a _) = typ a

instance (Typed a) => Typed (MemberDecl a) where
    typ (TMemTHint a _) = typ a
    typ (TMemFDecl a _) = typ a
