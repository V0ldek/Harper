module Harper.Interpreter.PatternMatching
    ( patMatchExpr
    , patMatchObj
    )
where
import qualified Data.Map                      as Map

import           Harper.Abs
import           Harper.Abs.Tuple
import           Harper.Interpreter.Alloc
import           Harper.Interpreter.Core
import           Harper.Interpreter.Declarations
import           Harper.Interpreter.Thunk

-- Continuation passing style - kMatch is called when object matches the pattern, kElse otherwise.
patMatchExpr
    :: Eval
    -> Pattern Meta
    -> Expression Meta
    -> Interpreter a
    -> Interpreter a
    -> Interpreter a
patMatchExpr _    PatDisc{}  _ kMatch _     = kMatch
patMatchExpr eval p@PatLit{} e kMatch kElse = do
    o <- eval e
    patMatchObj p o kMatch kElse
patMatchExpr eval p@PatTup{} e kMatch kElse = do
    o <- eval e
    patMatchObj p o kMatch kElse
patMatchExpr eval (PatDecl _ decl) e kMatch _ = do
    (l, _) <- makeThunk e eval
    env <- declLocal decl l
    localObjs (const env) kMatch
patMatchExpr eval p@PatCtor{} e kMatch kElse = do
    o <- eval e
    patMatchObj p o kMatch kElse
patMatchExpr _ p _ _ _ =
    error $ "Pattern matching this type of patterns is unsupported: " ++ show p

patMatchObj
    :: Pattern Meta -> Object -> Interpreter a -> Interpreter a -> Interpreter a
patMatchObj PatDisc{}      _ kMatch _     = kMatch
patMatchObj (PatLit _ lit) o kMatch kElse = do
    o' <- evalObj o
    case (lit, o') of
        (IntLit _ n1, Just (PInt n2)) | n1 == n2   -> kMatch
        (BoolLit _ (BTrue  _), Just (PBool True) ) -> kMatch
        (BoolLit _ (BFalse _), Just (PBool False)) -> kMatch
        (CharLit _ c1, Just (PChar c2)) | c1 == c2 -> kMatch
        (StrLit _ s1, Just (PStr s2)) | s1 == s2   -> kMatch
        (UnitLit _, Just PUnit)                    -> kMatch
        _                                          -> kElse
patMatchObj (PatTup _ tup) o kMatch kElse = do
    let pats = patTupToList tup
    o' <- evalObj o
    case o' of
        Just (Tup os) | length os == length pats ->
            patMatchSeq pats os kMatch kElse
        _ -> kElse
  where
    patMatchSeq [] [] kMatch _ = kMatch
    patMatchSeq (pat : pats) (o : os) kMatch kElse =
        patMatchObj pat o (patMatchSeq pats os kMatch kElse) kElse
patMatchObj (PatDecl _ decl) o kMatch _ = do
    l   <- alloc o
    env <- declLocal decl l
    localObjs (const env) kMatch
patMatchObj p@(PatCtor _ c flds) o kMatch kElse = do
    o' <- evalObj o
    case o' of
        Just v@(Inst t _) | ctorName t == c -> matchFlds v flds kMatch kElse
        _ -> kElse
  where
    matchFlds v@(Inst t d) (PatFld _ i p : flds) kMatch kElse = do
        let ptr = d Map.! i
        o <- getByPtr ptr
        patMatchObj p o (matchFlds v flds kMatch kElse) kElse
    matchFlds v [] kMatch _ = kMatch
patMatchObj p _ _ _ =
    error $ "Pattern matching this type of patterns is unsupported: " ++ show p
