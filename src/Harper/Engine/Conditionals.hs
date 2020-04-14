module Harper.Engine.Conditionals (
    linearizeCond
)
where

import Harper.Abs
import Harper.Engine.Values

-- Turns a conditional statement into a linear conditional statement.
-- Linear means a list of if statements where we assume that the first if with a true predicate
-- will be the only one to execute.
linearizeCond :: ConditionalStatement -> ConditionalStatement
linearizeCond s@(LinCondStmt _) = s
linearizeCond (IfElifStmts _if elifs) = LinCondStmt (_if:map elifToIf elifs)
linearizeCond (IfElifElseStmts _if elifs _else) = LinCondStmt (_if:map elifToIf elifs ++ [elseToIf _else])

elifToIf :: ElseIfStatement -> IfStatement
elifToIf (ElifStmt v s) = IfStmt v s

elseToIf :: ElseStatement -> IfStatement
elseToIf (ElseStmt s) = IfStmt litTrue s