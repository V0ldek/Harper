module Harper.Abs where

-- Haskell module generated by the BNF converter

newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
newtype UIdent = UIdent String deriving (Eq, Ord, Show, Read)
data Program a = Prog a [TopLvlDecl a]
  deriving (Eq, Ord, Show, Read)

instance Functor Program where
    fmap f x = case x of
        Prog a toplvldecls -> Prog (f a) (map (fmap f) toplvldecls)
data TopLvlDecl a
    = TopLvlFDecl a (FunDecl a)
    | TopLvlTHint a (TypeHint a)
    | TopLvlTDecl a (TypeDecl a)
  deriving (Eq, Ord, Show, Read)

instance Functor TopLvlDecl where
    fmap f x = case x of
        TopLvlFDecl a fundecl -> TopLvlFDecl (f a) (fmap f fundecl)
        TopLvlTHint a typehint -> TopLvlTHint (f a) (fmap f typehint)
        TopLvlTDecl a typedecl -> TopLvlTDecl (f a) (fmap f typedecl)
data TypeHint a = THint a Ident (TypeExpr a)
  deriving (Eq, Ord, Show, Read)

instance Functor TypeHint where
    fmap f x = case x of
        THint a ident typeexpr -> THint (f a) ident (fmap f typeexpr)
data TypeExpr a
    = TVar a Ident
    | TCtor a UIdent
    | TPur a (TypePurity a)
    | TUnit a
    | TTup a (TupleType a)
    | TAdHoc a [FieldTypeExpr a]
    | TApp a UIdent [TypeExpr a]
    | TFun a (TypeExpr a) (TypeExpr a)
  deriving (Eq, Ord, Show, Read)

instance Functor TypeExpr where
    fmap f x = case x of
        TVar a ident -> TVar (f a) ident
        TCtor a uident -> TCtor (f a) uident
        TPur a typepurity -> TPur (f a) (fmap f typepurity)
        TUnit a -> TUnit (f a)
        TTup a tupletype -> TTup (f a) (fmap f tupletype)
        TAdHoc a fieldtypeexprs -> TAdHoc (f a) (map (fmap f) fieldtypeexprs)
        TApp a uident typeexprs -> TApp (f a) uident (map (fmap f) typeexprs)
        TFun a typeexpr1 typeexpr2 -> TFun (f a) (fmap f typeexpr1) (fmap f typeexpr2)
data TupleType a
    = TTupList a (TypeExpr a) (TupleType a)
    | TTupTail a (TypeExpr a) (TypeExpr a)
  deriving (Eq, Ord, Show, Read)

instance Functor TupleType where
    fmap f x = case x of
        TTupList a typeexpr tupletype -> TTupList (f a) (fmap f typeexpr) (fmap f tupletype)
        TTupTail a typeexpr1 typeexpr2 -> TTupTail (f a) (fmap f typeexpr1) (fmap f typeexpr2)
data TypePurity a = TImpure a | TSideE a
  deriving (Eq, Ord, Show, Read)

instance Functor TypePurity where
    fmap f x = case x of
        TImpure a -> TImpure (f a)
        TSideE a -> TSideE (f a)
data FieldTypeExpr a = TFld a (TypeHint a)
  deriving (Eq, Ord, Show, Read)

instance Functor FieldTypeExpr where
    fmap f x = case x of
        TFld a typehint -> TFld (f a) (fmap f typehint)
data FunDecl a = FDecl a Ident [FunParam a] (FunBody a)
  deriving (Eq, Ord, Show, Read)

instance Functor FunDecl where
    fmap f x = case x of
        FDecl a ident funparams funbody -> FDecl (f a) ident (map (fmap f) funparams) (fmap f funbody)
data FunParam a = FParam a Ident
  deriving (Eq, Ord, Show, Read)

instance Functor FunParam where
    fmap f x = case x of
        FParam a ident -> FParam (f a) ident
data LambdaParam a = LamParam a (Pattern a)
  deriving (Eq, Ord, Show, Read)

instance Functor LambdaParam where
    fmap f x = case x of
        LamParam a pattern -> LamParam (f a) (fmap f pattern)
data FunBody a
    = FExprBody a (Expression a)
    | FStmtBody a (Statement a)
    | FVIterBody a (Statement a)
    | FRIterBody a (Statement a)
  deriving (Eq, Ord, Show, Read)

instance Functor FunBody where
    fmap f x = case x of
        FExprBody a expression -> FExprBody (f a) (fmap f expression)
        FStmtBody a statement -> FStmtBody (f a) (fmap f statement)
        FVIterBody a statement -> FVIterBody (f a) (fmap f statement)
        FRIterBody a statement -> FRIterBody (f a) (fmap f statement)
data BoolLiteral a = BTrue a | BFalse a
  deriving (Eq, Ord, Show, Read)

instance Functor BoolLiteral where
    fmap f x = case x of
        BTrue a -> BTrue (f a)
        BFalse a -> BFalse (f a)
data Literal a
    = UnitLit a
    | IntLit a Integer
    | CharLit a Char
    | StrLit a String
    | BoolLit a (BoolLiteral a)
  deriving (Eq, Ord, Show, Read)

instance Functor Literal where
    fmap f x = case x of
        UnitLit a -> UnitLit (f a)
        IntLit a integer -> IntLit (f a) integer
        CharLit a char -> CharLit (f a) char
        StrLit a string -> StrLit (f a) string
        BoolLit a boolliteral -> BoolLit (f a) (fmap f boolliteral)
data MemberAccess a = MembAcc a Ident
  deriving (Eq, Ord, Show, Read)

instance Functor MemberAccess where
    fmap f x = case x of
        MembAcc a ident -> MembAcc (f a) ident
data Expression a
    = ThisExpr a
    | TupExpr a (TupleExpression a)
    | LitExpr a (Literal a)
    | ObjExpr a Ident
    | TMembExpr a UIdent [MemberAccess a]
    | MembExpr a (Expression a) [MemberAccess a]
    | DataExpr a [MemberAccess a]
    | AdHocExpr a [AdHocFieldDecl a]
    | VCtorExpr a UIdent [FieldAss a]
    | MatchExpr a (Expression a) [MatchExpressionClause a]
    | AppExpr a (Expression a) (Expression a)
    | CompExpr a (Expression a) (Expression a)
    | PowExpr a (Expression a) (Expression a)
    | MulExpr a (Expression a) (Expression a)
    | DivExpr a (Expression a) (Expression a)
    | ModExpr a (Expression a) (Expression a)
    | AddExpr a (Expression a) (Expression a)
    | SubExpr a (Expression a) (Expression a)
    | NotExpr a (Expression a)
    | NegExpr a (Expression a)
    | EqExpr a (Expression a) (Expression a)
    | NEqExpr a (Expression a) (Expression a)
    | LtExpr a (Expression a) (Expression a)
    | GtExpr a (Expression a) (Expression a)
    | LEqExpr a (Expression a) (Expression a)
    | GEqExpr a (Expression a) (Expression a)
    | AndExpr a (Expression a) (Expression a)
    | OrExpr a (Expression a) (Expression a)
    | SeqExpr a (Expression a) (Expression a)
    | LamExpr a [LambdaParam a] (FunBody a)
  deriving (Eq, Ord, Show, Read)

instance Functor Expression where
    fmap f x = case x of
        ThisExpr a -> ThisExpr (f a)
        TupExpr a tupleexpression -> TupExpr (f a) (fmap f tupleexpression)
        LitExpr a literal -> LitExpr (f a) (fmap f literal)
        ObjExpr a ident -> ObjExpr (f a) ident
        TMembExpr a uident memberaccesss -> TMembExpr (f a) uident (map (fmap f) memberaccesss)
        MembExpr a expression memberaccesss -> MembExpr (f a) (fmap f expression) (map (fmap f) memberaccesss)
        DataExpr a memberaccesss -> DataExpr (f a) (map (fmap f) memberaccesss)
        AdHocExpr a adhocfielddecls -> AdHocExpr (f a) (map (fmap f) adhocfielddecls)
        VCtorExpr a uident fieldasss -> VCtorExpr (f a) uident (map (fmap f) fieldasss)
        MatchExpr a expression matchexpressionclauses -> MatchExpr (f a) (fmap f expression) (map (fmap f) matchexpressionclauses)
        AppExpr a expression1 expression2 -> AppExpr (f a) (fmap f expression1) (fmap f expression2)
        CompExpr a expression1 expression2 -> CompExpr (f a) (fmap f expression1) (fmap f expression2)
        PowExpr a expression1 expression2 -> PowExpr (f a) (fmap f expression1) (fmap f expression2)
        MulExpr a expression1 expression2 -> MulExpr (f a) (fmap f expression1) (fmap f expression2)
        DivExpr a expression1 expression2 -> DivExpr (f a) (fmap f expression1) (fmap f expression2)
        ModExpr a expression1 expression2 -> ModExpr (f a) (fmap f expression1) (fmap f expression2)
        AddExpr a expression1 expression2 -> AddExpr (f a) (fmap f expression1) (fmap f expression2)
        SubExpr a expression1 expression2 -> SubExpr (f a) (fmap f expression1) (fmap f expression2)
        NotExpr a expression -> NotExpr (f a) (fmap f expression)
        NegExpr a expression -> NegExpr (f a) (fmap f expression)
        EqExpr a expression1 expression2 -> EqExpr (f a) (fmap f expression1) (fmap f expression2)
        NEqExpr a expression1 expression2 -> NEqExpr (f a) (fmap f expression1) (fmap f expression2)
        LtExpr a expression1 expression2 -> LtExpr (f a) (fmap f expression1) (fmap f expression2)
        GtExpr a expression1 expression2 -> GtExpr (f a) (fmap f expression1) (fmap f expression2)
        LEqExpr a expression1 expression2 -> LEqExpr (f a) (fmap f expression1) (fmap f expression2)
        GEqExpr a expression1 expression2 -> GEqExpr (f a) (fmap f expression1) (fmap f expression2)
        AndExpr a expression1 expression2 -> AndExpr (f a) (fmap f expression1) (fmap f expression2)
        OrExpr a expression1 expression2 -> OrExpr (f a) (fmap f expression1) (fmap f expression2)
        SeqExpr a expression1 expression2 -> SeqExpr (f a) (fmap f expression1) (fmap f expression2)
        LamExpr a lambdaparams funbody -> LamExpr (f a) (map (fmap f) lambdaparams) (fmap f funbody)
data TupleExpression a
    = TupExprList a (Expression a) (TupleExpression a)
    | TupExprTail a (Expression a) (Expression a)
  deriving (Eq, Ord, Show, Read)

instance Functor TupleExpression where
    fmap f x = case x of
        TupExprList a expression tupleexpression -> TupExprList (f a) (fmap f expression) (fmap f tupleexpression)
        TupExprTail a expression1 expression2 -> TupExprTail (f a) (fmap f expression1) (fmap f expression2)
data MatchExpressionClause a
    = MatchExprClause a (Pattern a) (Expression a)
  deriving (Eq, Ord, Show, Read)

instance Functor MatchExpressionClause where
    fmap f x = case x of
        MatchExprClause a pattern expression -> MatchExprClause (f a) (fmap f pattern) (fmap f expression)
data FieldAss a = DataAss a Ident (Expression a)
  deriving (Eq, Ord, Show, Read)

instance Functor FieldAss where
    fmap f x = case x of
        DataAss a ident expression -> DataAss (f a) ident (fmap f expression)
data Statement a
    = EmptyStmt a
    | StmtBlock a [Statement a]
    | StmtBlockWDecls a [Statement a] [LocalFunDecl a]
    | RetStmt a
    | RetExprStmt a (Expression a)
    | CntStmt a
    | BrkStmt a
    | YieldStmt a (Expression a)
    | YieldRetStmt a
    | MatchStmt a (Expression a) [MatchStatementClause a]
    | WhileStmt a (Expression a) (Statement a)
    | ForInStmt a (Pattern a) (Expression a) (Statement a)
    | CondStmt a (ConditionalStatement a)
    | DconStmt a (Pattern a) (Expression a)
    | DeclStmt a (LocalObjDecl a)
    | AssStmt a Ident (Expression a)
    | AddStmt a Ident (Expression a)
    | SubStmt a Ident (Expression a)
    | MulStmt a Ident (Expression a)
    | DivStmt a Ident (Expression a)
    | PowStmt a Ident (Expression a)
    | CompStmt a Ident (Expression a)
    | DataAddStmt a Ident (Expression a)
    | DataSubStmt a Ident (Expression a)
    | DataMulStmt a Ident (Expression a)
    | DataDivStmt a Ident (Expression a)
    | DataAssStmt a Ident (Expression a)
    | DataPowStmt a Ident (Expression a)
    | DataCompStmt a Ident (Expression a)
    | EvalStmt a (Expression a)
  deriving (Eq, Ord, Show, Read)

instance Functor Statement where
    fmap f x = case x of
        EmptyStmt a -> EmptyStmt (f a)
        StmtBlock a statements -> StmtBlock (f a) (map (fmap f) statements)
        StmtBlockWDecls a statements localfundecls -> StmtBlockWDecls (f a) (map (fmap f) statements) (map (fmap f) localfundecls)
        RetStmt a -> RetStmt (f a)
        RetExprStmt a expression -> RetExprStmt (f a) (fmap f expression)
        CntStmt a -> CntStmt (f a)
        BrkStmt a -> BrkStmt (f a)
        YieldStmt a expression -> YieldStmt (f a) (fmap f expression)
        YieldRetStmt a -> YieldRetStmt (f a)
        MatchStmt a expression matchstatementclauses -> MatchStmt (f a) (fmap f expression) (map (fmap f) matchstatementclauses)
        WhileStmt a expression statement -> WhileStmt (f a) (fmap f expression) (fmap f statement)
        ForInStmt a pattern expression statement -> ForInStmt (f a) (fmap f pattern) (fmap f expression) (fmap f statement)
        CondStmt a conditionalstatement -> CondStmt (f a) (fmap f conditionalstatement)
        DconStmt a pattern expression -> DconStmt (f a) (fmap f pattern) (fmap f expression)
        DeclStmt a localobjdecl -> DeclStmt (f a) (fmap f localobjdecl)
        AssStmt a ident expression -> AssStmt (f a) ident (fmap f expression)
        AddStmt a ident expression -> AddStmt (f a) ident (fmap f expression)
        SubStmt a ident expression -> SubStmt (f a) ident (fmap f expression)
        MulStmt a ident expression -> MulStmt (f a) ident (fmap f expression)
        DivStmt a ident expression -> DivStmt (f a) ident (fmap f expression)
        PowStmt a ident expression -> PowStmt (f a) ident (fmap f expression)
        CompStmt a ident expression -> CompStmt (f a) ident (fmap f expression)
        DataAddStmt a ident expression -> DataAddStmt (f a) ident (fmap f expression)
        DataSubStmt a ident expression -> DataSubStmt (f a) ident (fmap f expression)
        DataMulStmt a ident expression -> DataMulStmt (f a) ident (fmap f expression)
        DataDivStmt a ident expression -> DataDivStmt (f a) ident (fmap f expression)
        DataAssStmt a ident expression -> DataAssStmt (f a) ident (fmap f expression)
        DataPowStmt a ident expression -> DataPowStmt (f a) ident (fmap f expression)
        DataCompStmt a ident expression -> DataCompStmt (f a) ident (fmap f expression)
        EvalStmt a expression -> EvalStmt (f a) (fmap f expression)
data MatchStatementClause a
    = MatchStmtClause a (Pattern a) (Statement a)
  deriving (Eq, Ord, Show, Read)

instance Functor MatchStatementClause where
    fmap f x = case x of
        MatchStmtClause a pattern statement -> MatchStmtClause (f a) (fmap f pattern) (fmap f statement)
data ConditionalStatement a
    = IfElifStmts a (IfStatement a) [ElseIfStatement a]
    | IfElifElseStmts a (IfStatement a) [ElseIfStatement a] (ElseStatement a)
  deriving (Eq, Ord, Show, Read)

instance Functor ConditionalStatement where
    fmap f x = case x of
        IfElifStmts a ifstatement elseifstatements -> IfElifStmts (f a) (fmap f ifstatement) (map (fmap f) elseifstatements)
        IfElifElseStmts a ifstatement elseifstatements elsestatement -> IfElifElseStmts (f a) (fmap f ifstatement) (map (fmap f) elseifstatements) (fmap f elsestatement)
data IfStatement a = IfStmt a (Expression a) (Statement a)
  deriving (Eq, Ord, Show, Read)

instance Functor IfStatement where
    fmap f x = case x of
        IfStmt a expression statement -> IfStmt (f a) (fmap f expression) (fmap f statement)
data ElseIfStatement a = ElifStmt a (Expression a) (Statement a)
  deriving (Eq, Ord, Show, Read)

instance Functor ElseIfStatement where
    fmap f x = case x of
        ElifStmt a expression statement -> ElifStmt (f a) (fmap f expression) (fmap f statement)
data ElseStatement a = ElseStmt a (Statement a)
  deriving (Eq, Ord, Show, Read)

instance Functor ElseStatement where
    fmap f x = case x of
        ElseStmt a statement -> ElseStmt (f a) (fmap f statement)
data Pattern a
    = PatLit a (Literal a)
    | PatDecl a (LocalObjDecl a)
    | PatData a [FieldPattern a]
    | PatTup a (TuplePattern a)
    | PatDisc a
    | PatCtor a UIdent [FieldPattern a]
  deriving (Eq, Ord, Show, Read)

instance Functor Pattern where
    fmap f x = case x of
        PatLit a literal -> PatLit (f a) (fmap f literal)
        PatDecl a localobjdecl -> PatDecl (f a) (fmap f localobjdecl)
        PatData a fieldpatterns -> PatData (f a) (map (fmap f) fieldpatterns)
        PatTup a tuplepattern -> PatTup (f a) (fmap f tuplepattern)
        PatDisc a -> PatDisc (f a)
        PatCtor a uident fieldpatterns -> PatCtor (f a) uident (map (fmap f) fieldpatterns)
data TuplePattern a
    = PatTupList a (Pattern a) (TuplePattern a)
    | PatTupTail a (Pattern a) (Pattern a)
  deriving (Eq, Ord, Show, Read)

instance Functor TuplePattern where
    fmap f x = case x of
        PatTupList a pattern tuplepattern -> PatTupList (f a) (fmap f pattern) (fmap f tuplepattern)
        PatTupTail a pattern1 pattern2 -> PatTupTail (f a) (fmap f pattern1) (fmap f pattern2)
data FieldPattern a = PatFld a Ident (Pattern a)
  deriving (Eq, Ord, Show, Read)

instance Functor FieldPattern where
    fmap f x = case x of
        PatFld a ident pattern -> PatFld (f a) ident (fmap f pattern)
data Declaration a = Decl a Ident | DeclWHint a (TypeHint a)
  deriving (Eq, Ord, Show, Read)

instance Functor Declaration where
    fmap f x = case x of
        Decl a ident -> Decl (f a) ident
        DeclWHint a typehint -> DeclWHint (f a) (fmap f typehint)
data AdHocFieldDecl a = AdHocFld a (Declaration a) (Expression a)
  deriving (Eq, Ord, Show, Read)

instance Functor AdHocFieldDecl where
    fmap f x = case x of
        AdHocFld a declaration expression -> AdHocFld (f a) (fmap f declaration) (fmap f expression)
data LocalFunDecl a
    = LocTHint a (TypeHint a) | LocFDecl a (FunDecl a)
  deriving (Eq, Ord, Show, Read)

instance Functor LocalFunDecl where
    fmap f x = case x of
        LocTHint a typehint -> LocTHint (f a) (fmap f typehint)
        LocFDecl a fundecl -> LocFDecl (f a) (fmap f fundecl)
data LocalObjDecl a
    = LocVarDecl a (Declaration a) | LocValDecl a (Declaration a)
  deriving (Eq, Ord, Show, Read)

instance Functor LocalObjDecl where
    fmap f x = case x of
        LocVarDecl a declaration -> LocVarDecl (f a) (fmap f declaration)
        LocValDecl a declaration -> LocValDecl (f a) (fmap f declaration)
data TypeSignature a = TSig a UIdent [TypeParameter a]
  deriving (Eq, Ord, Show, Read)

instance Functor TypeSignature where
    fmap f x = case x of
        TSig a uident typeparameters -> TSig (f a) uident (map (fmap f) typeparameters)
data TypeDecl a
    = ValTDecl a (TypeSignature a) (TypeBody a)
    | ValTUDecl a (TypeSignature a) [TypeVariantDecl a]
    | RefTDecl a (TypeSignature a) (TypeBody a)
  deriving (Eq, Ord, Show, Read)

instance Functor TypeDecl where
    fmap f x = case x of
        ValTDecl a typesignature typebody -> ValTDecl (f a) (fmap f typesignature) (fmap f typebody)
        ValTUDecl a typesignature typevariantdecls -> ValTUDecl (f a) (fmap f typesignature) (map (fmap f) typevariantdecls)
        RefTDecl a typesignature typebody -> RefTDecl (f a) (fmap f typesignature) (fmap f typebody)
data TypeParameter a = TParam a Ident
  deriving (Eq, Ord, Show, Read)

instance Functor TypeParameter where
    fmap f x = case x of
        TParam a ident -> TParam (f a) ident
data TypeVariantDecl a = TVarDecl a UIdent (TypeBody a)
  deriving (Eq, Ord, Show, Read)

instance Functor TypeVariantDecl where
    fmap f x = case x of
        TVarDecl a uident typebody -> TVarDecl (f a) uident (fmap f typebody)
data TypeBody a
    = DataTBody a [FieldDecl a] [MemberDecl a] | TBody a [MemberDecl a]
  deriving (Eq, Ord, Show, Read)

instance Functor TypeBody where
    fmap f x = case x of
        DataTBody a fielddecls memberdecls -> DataTBody (f a) (map (fmap f) fielddecls) (map (fmap f) memberdecls)
        TBody a memberdecls -> TBody (f a) (map (fmap f) memberdecls)
data FieldDecl a = TFldDecl a (TypeHint a)
  deriving (Eq, Ord, Show, Read)

instance Functor FieldDecl where
    fmap f x = case x of
        TFldDecl a typehint -> TFldDecl (f a) (fmap f typehint)
data MemberDecl a
    = TMemTHint a (TypeHint a) | TMemFDecl a (FunDecl a)
  deriving (Eq, Ord, Show, Read)

instance Functor MemberDecl where
    fmap f x = case x of
        TMemTHint a typehint -> TMemTHint (f a) (fmap f typehint)
        TMemFDecl a fundecl -> TMemFDecl (f a) (fmap f fundecl)
