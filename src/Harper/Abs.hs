module Harper.Abs where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
newtype UIdent = UIdent String deriving (Eq, Ord, Show, Read)
data Program = Prog [TopLvlDecl]
  deriving (Eq, Ord, Show, Read)

data TopLvlDecl
    = TopLvlFDecl FunDecl | TopLvlTHint TypeHint | TopLvlTDecl TypeDecl
  deriving (Eq, Ord, Show, Read)

data TypeHint = THint Ident TypeExpr
  deriving (Eq, Ord, Show, Read)

data Declaration = Decl Ident | DeclWHint TypeHint
  deriving (Eq, Ord, Show, Read)

data TupleType
    = TTupList TypeExpr TupleType | TTupTail TypeExpr TypeExpr
  deriving (Eq, Ord, Show, Read)

data TypePurity = TImpure | TSideE
  deriving (Eq, Ord, Show, Read)

data FieldTypeExpr = TFld TypeHint
  deriving (Eq, Ord, Show, Read)

data TypeExpr
    = TVar Ident
    | TCtor UIdent
    | TPur TypePurity
    | TUnit
    | TTup TupleType
    | TAdHoc [FieldTypeExpr]
    | TApp TypeExpr TypeExpr
    | TFun TypeExpr TypeExpr
  deriving (Eq, Ord, Show, Read)

data FunDecl = FDecl Ident [FunArg] FunBody
  deriving (Eq, Ord, Show, Read)

data FunArg = FArg Ident
  deriving (Eq, Ord, Show, Read)

data LambdaArg = LamArg Pattern
  deriving (Eq, Ord, Show, Read)

data FunBody = FValBody Value | FStmtBody Statement
  deriving (Eq, Ord, Show, Read)

data BoolLiteral = BTrue | BFalse
  deriving (Eq, Ord, Show, Read)

data Literal
    = IntLit Integer
    | CharLit Char
    | StrLit String
    | BoolLit BoolLiteral
  deriving (Eq, Ord, Show, Read)

data TupleValue
    = TupValList Value TupleValue | TupValTail Value Value
  deriving (Eq, Ord, Show, Read)

data Qualifier
    = Qual Ident
    | Quals Qualifier Qualifier
    | ThisQual
    | DataQual
    | NewQual
  deriving (Eq, Ord, Show, Read)

data Value
    = ThisVal
    | AdHocVal [AdHocFieldDecl]
    | VCtorVal UIdent [FieldAss]
    | TupVal TupleValue
    | LitVal Literal
    | ObjVal Ident
    | CtorVal UIdent
    | QObjVal Qualifier Ident
    | UnitVal
    | MatchVal Value [MatchValueClause]
    | AppVal Value Value
    | CompVal Value Value
    | PowVal Value Value
    | MulVal Value Value
    | DivVal Value Value
    | ModVal Value Value
    | AddVal Value Value
    | SubVal Value Value
    | NotVal Value
    | EqVal Value Value
    | NEqVal Value Value
    | LessVal Value Value
    | GreaterVal Value Value
    | LEqVal Value Value
    | GEqVal Value Value
    | AndVal Value Value
    | OrVal Value Value
    | LamVal [LambdaArg] FunBody
    | SeqVal Value Value
  deriving (Eq, Ord, Show, Read)

data MatchValueClause = MatchValClause Pattern Value
  deriving (Eq, Ord, Show, Read)

data AdHocFieldDecl = AdHocFld Declaration Value
  deriving (Eq, Ord, Show, Read)

data FieldAss = DataAss Ident Value
  deriving (Eq, Ord, Show, Read)

data Statement
    = EmptyStmt
    | StmtBlock [Statement]
    | StmtBlockWDecls [Statement] [LocalFunDecl]
    | RetStmt
    | RetValStmt Value
    | CntStmt
    | BrkStmt
    | YieldStmt Value
    | MatchStmt Value [MatchStatementClause]
    | WhileStmt Value Statement
    | ForInStmt Pattern Value Statement
    | CondStmt ConditionalStatement
    | DconStmt Pattern Value
    | DeclStmt LocalObjDecl
    | AssStmt Ident Value
    | AddStmt Ident Value
    | SubStmt Ident Value
    | MulStmt Ident Value
    | DivStmt Ident Value
    | PowStmt Ident Value
    | CompStmt Ident Value
    | QAssStmt Qualifier Ident Value
    | QAddStmt Qualifier Ident Value
    | QSubStmt Qualifier Ident Value
    | QMulStmt Qualifier Ident Value
    | QDivStmt Qualifier Ident Value
    | QPowStmt Qualifier Ident Value
    | QCompStmt Qualifier Ident Value
    | EvalStmt Value
  deriving (Eq, Ord, Show, Read)

data MatchStatementClause = MatchStmtClause Pattern Statement
  deriving (Eq, Ord, Show, Read)

data ConditionalStatement
    = IfElifStmts IfStatement [ElseIfStatement]
    | IfElifElseStmts IfStatement [ElseIfStatement] ElseStatement
    | LinCondStmt [IfStatement] -- Internal transformation.
  deriving (Eq, Ord, Show, Read)

data IfStatement = IfStmt Value Statement
  deriving (Eq, Ord, Show, Read)

data ElseIfStatement = ElifStmt Value Statement
  deriving (Eq, Ord, Show, Read)

data ElseStatement = ElseStmt Statement
  deriving (Eq, Ord, Show, Read)

data VarSpecifier = LocSVar | LocSVal
  deriving (Eq, Ord, Show, Read)

data LocalFunDecl = LocTHint TypeHint | LocFDecl FunDecl
  deriving (Eq, Ord, Show, Read)

data LocalObjDecl = LocVDecl VarSpecifier Declaration
  deriving (Eq, Ord, Show, Read)

data Pattern
    = PatDecl LocalObjDecl
    | PatData [FieldPattern]
    | PatTup TuplePattern
    | PatDisc
    | PatCtor UIdent [FieldPattern]
  deriving (Eq, Ord, Show, Read)

data TuplePattern
    = PatTupList Pattern TuplePattern | PatTupTail Pattern Pattern
  deriving (Eq, Ord, Show, Read)

data FieldPattern = PatFld Ident Pattern
  deriving (Eq, Ord, Show, Read)

data TypeSignature = TSig UIdent [TypeArgument]
  deriving (Eq, Ord, Show, Read)

data TypeDecl
    = ValTDecl TypeSignature TypeBody
    | RefTDecl TypeSignature TypeBody
    | ValTUDecl TypeSignature [TypeVariantDecl]
  deriving (Eq, Ord, Show, Read)

data TypeArgument = TArg Ident
  deriving (Eq, Ord, Show, Read)

data TypeVariantDecl = TVarDecl UIdent TypeBody
  deriving (Eq, Ord, Show, Read)

data TypeBody
    = DataTBody [FieldDecl] [MemberDecl] | TBody [MemberDecl]
  deriving (Eq, Ord, Show, Read)

data FieldDecl = TFldDecl TypeHint
  deriving (Eq, Ord, Show, Read)

data MemberDecl = TMemTHint TypeHint | TMemFDecl FunDecl
  deriving (Eq, Ord, Show, Read)

