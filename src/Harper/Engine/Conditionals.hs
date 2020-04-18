module Harper.Engine.Conditionals (
    linearizeCond
)
where

import Harper.Abs
import Harper.Engine.Values

-- Turns a conditional statement into a linear conditional statement.
-- Linear means a list of if statements where we assume that the first if with a true predicate
-- will be the only one to execute.
linearizeCond :: ConditionalStatement Pos -> [IfStatement Pos]
linearizeCond (IfElifStmts _ _if elifs) = _if:map elifToIf elifs
linearizeCond (IfElifElseStmts _ _if elifs _else) = _if:map elifToIf elifs ++ [elseToIf _else]

elifToIf :: ElseIfStatement Pos -> IfStatement Pos
elifToIf (ElifStmt a v s) = IfStmt a v s

elseToIf :: ElseStatement Pos -> IfStatement Pos
elseToIf (ElseStmt a s) = IfStmt a litTrue s