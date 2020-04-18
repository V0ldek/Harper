{-# LANGUAGE FlexibleInstances #-}
module Harper.Abs.Pos
where

import Harper.Abs

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

instance Position (Declaration Pos) where
    pos (Decl a _)      = a
    pos (DeclWHint a _) = a

instance Position (TupleType Pos) where
    pos (TTupList a _ _) = a
    pos (TTupTail a _ _) = a

instance Position (TypePurity Pos) where
    pos (TImpure a) = a
    pos (TSideE a)  = a
    
instance Position (FieldTypeExpr Pos) where
    pos (TFld a _) = a

instance Position (TypeExpr Pos) where
    pos (TVar a _)   = a
    pos (TCtor a _)  = a
    pos (TPur a _)   = a
    pos (TUnit a)    = a
    pos (TTup a _)   = a
    pos (TAdHoc a _) = a
    pos (TApp a _ _) = a
    pos (TFun a _ _) = a

instance Position (FunDecl Pos) where
    pos (FDecl a _ _ _) = a

instance Position (FunArg Pos) where
    pos (FArg a _) = a

instance Position (LambdaArg Pos) where
    pos (LamArg a _) = a

instance Position (FunBody Pos) where
    pos (FValBody a _)  = a
    pos (FStmtBody a _) = a

instance Position (BoolLiteral Pos) where
    pos (BTrue a)  = a
    pos (BFalse a) = a

instance Position (Literal Pos) where
    pos (IntLit a _)  = a
    pos (CharLit a _) = a
    pos (StrLit a _)  = a
    pos (BoolLit a _) = a

instance Position (TupleValue Pos) where
    pos (TupValList a _ _) = a
    pos (TupValTail a _ _) = a
    
instance Position (Qualifier Pos) where
    pos (Qual a _)    = a
    pos (Quals a _ _) = a
    pos (ThisQual a)  = a
    pos (DataQual a)  = a
    pos (NewQual a)   = a

instance Position (Value Pos) where
    pos (ThisVal a)        = a
    pos (AdHocVal a _)     = a
    pos (VCtorVal a _ _)   = a
    pos (TupVal a _)       = a
    pos (LitVal a _)       = a
    pos (ObjVal a _)       = a
    pos (CtorVal a _)      = a
    pos (QObjVal a _ _)    = a
    pos (UnitVal a)        = a
    pos (MatchVal a _ _)   = a
    pos (AppVal a _ _)     = a
    pos (CompVal a _ _)    = a
    pos (PowVal a _ _)     = a
    pos (MulVal a _ _)     = a
    pos (DivVal a _ _)     = a
    pos (ModVal a _ _)     = a
    pos (AddVal a _ _)     = a
    pos (SubVal a _ _)     = a
    pos (NotVal a _)       = a
    pos (EqVal a _ _)      = a
    pos (NEqVal a _ _)     = a
    pos (LessVal a _ _)    = a
    pos (GreaterVal a _ _) = a
    pos (LEqVal a _ _)     = a
    pos (GEqVal a _ _)     = a
    pos (AndVal a _ _)     = a
    pos (OrVal a _ _)      = a
    pos (LamVal a _ _)     = a
    pos (SeqVal a _ _)     = a

instance Position (MatchValueClause Pos) where
    pos (MatchValClause a _ _) = a

instance Position (AdHocFieldDecl Pos) where
    pos (AdHocFld a _ _) = a

instance Position (FieldAss Pos) where
    pos (DataAss a _ _) = a

instance Position (Statement Pos) where
    pos (EmptyStmt a)           = a
    pos (StmtBlock a _)         = a
    pos (StmtBlockWDecls a _ _) = a
    pos (RetStmt a)             = a 
    pos (RetValStmt a _)        = a
    pos (CntStmt a)             = a
    pos (BrkStmt a)             = a
    pos (YieldStmt a _)         = a
    pos (MatchStmt a _ _)       = a
    pos (WhileStmt a _ _)       = a
    pos (ForInStmt a _ _ _)     = a
    pos (CondStmt a _)          = a
    pos (DconStmt a _ _)        = a
    pos (DeclStmt a _)          = a
    pos (AssStmt a _ _)         = a
    pos (AddStmt a _ _)         = a
    pos (SubStmt a _ _)         = a
    pos (MulStmt a _ _)         = a
    pos (DivStmt a _ _)         = a
    pos (PowStmt a _ _)         = a
    pos (CompStmt a _ _)        = a
    pos (QAssStmt a _ _ _)      = a
    pos (QAddStmt a _ _ _)      = a
    pos (QSubStmt a _ _ _)      = a
    pos (QMulStmt a _ _ _)      = a
    pos (QDivStmt a _ _ _)      = a
    pos (QPowStmt a _ _ _)      = a
    pos (QCompStmt a _ _ _)     = a
    pos (EvalStmt a _)          = a

instance Position (MatchStatementClause Pos) where
    pos (MatchStmtClause a _ _) = a

instance Position (ConditionalStatement Pos) where
    pos (IfElifStmts a _ _) = a
    pos (IfElifElseStmts a _ _ _) = a
    
instance Position (IfStatement Pos) where
    pos (IfStmt a _ _) = a

instance Position (ElseIfStatement Pos) where
    pos (ElifStmt a _ _) = a

instance Position (ElseStatement Pos) where
    pos (ElseStmt a _) = a

instance Position (VarSpecifier Pos) where
    pos (LocSVar a) = a
    pos (LocSVal a) = a

instance Position (LocalFunDecl Pos) where
    pos (LocTHint a _) = a
    pos (LocFDecl a _) = a

instance Position (LocalObjDecl Pos) where
    pos (LocVDecl a _ _) = a

instance Position (Pattern Pos) where
    pos (PatDecl a _)   = a
    pos (PatData a _)   = a
    pos (PatTup a _)    = a
    pos (PatDisc a)     = a
    pos (PatCtor a _ _) = a

instance Position (TuplePattern Pos) where
    pos (PatTupList a _ _) = a
    pos (PatTupTail a _ _) = a

instance Position (FieldPattern Pos) where
    pos (PatFld a _ _) = a

instance Position (TypeSignature Pos) where
    pos (TSig a _ _) = a

instance Position (TypeDecl Pos) where
    pos (ValTDecl a _ _)  = a
    pos (RefTDecl a _ _)  = a
    pos (ValTUDecl a _ _) = a

instance Position (TypeArgument Pos) where
    pos (TArg a _) = a

instance Position (TypeVariantDecl Pos) where
    pos (TVarDecl a _ _) = a

instance Position (TypeBody Pos) where
    pos (DataTBody a _ _) = a
    pos (TBody a _)       = a

instance Position (FieldDecl Pos) where
    pos (TFldDecl a _) = a

instance Position (MemberDecl Pos) where
    pos (TMemTHint a _) = a
    pos (TMemFDecl a _) = a
    