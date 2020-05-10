module Harper.Interpreter.Iterable
    ( forInIterable
    , forInRefIterable
    )
where

import           Harper.Abs
import           Harper.Expressions
import           Harper.Interpreter.Alloc
import           Harper.Interpreter.Core
import           Harper.TypeSystem.GlobalTypes

-- See issue #30 for these semantics written in Harper instead of raw AST.

forInIterable :: Statement Meta -> Interpreter (Statement Meta)
forInIterable f@(ForInVStmt a pat e s) = do
    vars <- newvars 4
    let [iterVar, hasNextVar, iter'Var, hasNext'Var] = vars
        iter          = ObjExpr a iterVar
        hasNext       = ObjExpr a hasNextVar
        iter'         = ObjExpr a iter'Var
        hasNext'      = ObjExpr a hasNext'Var
        iterateAccess = MembExpr a e [MembAcc a iterateI]
        nextAccess    = MembExpr a iter [MembAcc a iterNextI]
        currentAccess = MembExpr a iter [MembAcc a iterCurrentI]
        iterInit =
            DconStmt a (PatDecl a (LocVarDecl a (Decl a iterVar))) iterateAccess
        hasNextDecl =
            DconStmt a (PatDecl a (LocVarDecl a (Decl a hasNextVar))) litTrue
        updateStmt = DconStmt
            a
            (PatTup
                a
                (PatTupTail a
                            (PatDecl a (LocValDecl a (Decl a hasNext'Var)))
                            (PatDecl a (LocValDecl a (Decl a iter'Var)))
                )
            )
            nextAccess
        updateIter    = AssStmt a iterVar iter'
        updateHasNext = AssStmt a hasNextVar hasNext'
        initStmt = DconStmt a pat (MembExpr a iter [MembAcc a iterCurrentI])
        checkBreak    = CondStmt
            a
            (IfElifStmts a (IfStmt a (NotExpr a hasNext') (BrkStmt a)) [])
        whileBody = StmtBlock
            a
            [updateStmt, checkBreak, updateIter, updateHasNext, initStmt, s]
        whileStmt = WhileStmt a hasNext whileBody
        block     = StmtBlock a [iterInit, hasNextDecl, whileStmt]
    return block

forInRefIterable :: Statement Meta -> Interpreter (Statement Meta)
forInRefIterable f@(ForInRStmt a pat e s) = do
    iterVar <- newvar
    let iter          = ObjExpr a iterVar
        iterateAccess = MembExpr a e [MembAcc a iterateI]
        nextAccess    = MembExpr a iter [MembAcc a iterNextI]
        currentAccess = MembExpr a iter [MembAcc a iterCurrentI]
        unit          = LitExpr a (UnitLit a)
        pred          = AppExpr a nextAccess unit
        iterInit =
            DconStmt a (PatDecl a (LocValDecl a (Decl a iterVar))) iterateAccess
        initStmt  = DconStmt a pat (AppExpr a currentAccess unit)
        whileBody = StmtBlock a [initStmt, s]
        whileStmt = WhileStmt a pred whileBody
        block     = StmtBlock a [iterInit, whileStmt]
    return block
