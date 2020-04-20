import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit

import           Data.Char
import           Harper.Lexer
import           Harper.Parser
import           Harper.Engine
import           Harper.Engine.Core
import           Harper.Printer
import           ErrM
import           OutputM

data TestProgram = TProg String (Output String Object)

lexer = myLexer
parser = pProgram

issue1_sqr =
    TProg "\
\sqr n = n * n;\n\

\main = sqr 42;" (Out [] (Ok (PInt 1764)))

issue1_partial = TProg
    "\
\f a b c d = (c ^ d / a) * 123 + b;\n\

\fun n = f n;\n\

\main = (fun 2) 420 6 4;"
    (Out [] (Ok (PInt 80124)))

issue1_string = TProg "\
\main = \"abcd\";" (Out [] (Ok (PStr "\"abcd\"")))

issue1_bool1 = TProg
    "\
\f n = 2 * n;\n\

\main = (f 42 == 84) and (f 2 <= f 4) and (f 2 < f 5) and (true and not false);"
    (Out [] (Ok (PBool True)))

issue1_bool2 = TProg "\
\main = true or false;" (Out [] (Ok (PBool True)))

issue1_bool3 = TProg "\
\main = true and false;" (Out [] (Ok (PBool False)))

issue1_mod = TProg
    "\
\f n = 2 * n;\n\

\main = f 5 mod 3 == 1 and f 5 mod 3 != 2 and f 5 mod 3 > 0 and f 5 mod 3 >= 1;"
    (Out [] (Ok (PBool True)))

issue1_lazy = TProg "\
\fun n = fun (n + 1);\n\

\main = true or (fun 42);"
                    (Out [] (Ok (PBool True)))

issue2_fac = TProg
    "\
\fac n = {\
\  if n <= 0 {\
\    return 1;\
\  }\
\  else {\
\    return fac (n - 1) * n;\
\  }\
\};\
\\n\
\main = fac 42;\
\"
    (Out [] (Ok (PInt 1405006117752879898543142606244511569936384000000000)))

issue2_lam = TProg
    "\
\f n = {\
\  if n mod 2 == 0 {\
\    return (\\x y => x * y * n);\
\  }\
\  else if n == 37 {\
\    return (\\x y => 42);\
\  }\
\  else {\
\    return (\\x y => x + y + n);\
\  }\
\};\
\\n\
\main = f 42 3 4 == 504 and \
\       f 37 100 100 == 42 and \
\       f 37 1000 1000 == 42 and \
\       f 0 2 2 == 0 and \
\       f 1 2 3 == 6 and \
\       f 17 37 47 == 101;\
\"
    (Out [] (Ok (PBool True)))

issue2_lamPar = TProg
    "\
\f n = return (\\x y => n + x + y);\n\
\\n\
\appMul a b = (\\n => f n (a * b));\n\
\\n\
\main = appMul 7 17 42 5;\n\
\"
    (Out [] (Ok (PInt 166)))

issue3_divZero = TProg
    "\
\main = 42 / 0;\
\"
    (Out
        "\ESC[38;2;255;0;0mError: expression `0` evaluated to zero causing a division error.\n\
    \During evaluation of:\n\
    \ 42 / 0\n\
    \Located at line 1 column 8\ESC[38;2;255;255;255m\
    \"
        (Bad "runtime error.")
    )

issue3_modZero = TProg
    "\
\main = f 0;\n\
\\n\
\f n = 42 mod n;\
\"
    (Out
        "\ESC[38;2;255;0;0mError: expression `n` evaluated to zero causing a division error.\n\
    \During evaluation of:\n\
    \ 42 mod n\n\
    \Located at line 3 column 7\ESC[38;2;255;255;255m\
    \"
        (Bad "runtime error.")
    )

issue3_invType = TProg
    "\
\main = g 42;\n\
\\n\
\g n = f n \"Str\";\n\
\\n\
\f x y = x * y;\n\
\"
    (Out
        "\ESC[38;2;255;0;0mError: expression `y` has invalid type 'String'; expected 'Integer'.\n\
    \During evaluation of:\n\
    \ x * y\n\
    \Located at line 5 column 9\ESC[38;2;255;255;255m\
    \"
        (Bad "runtime error.")
    )

issue3_noReturn = TProg
    "\
\f n = {\n\
\  if n <= 17 {\n\
\    return 42;\n\
\  }\n\
\};\n\
\\n\
\main = (f 17) + (f 18);\n\
\"
    (Out
        "\ESC[38;2;255;0;0mError: control reached end of function without a return statement.\n\
    \During evaluation of:\n\
    \ {\n\
    \  if n <= 17 {\n\
    \    return 42 ;\n\
    \  }\n\
    \}\n\
    \Located at line 1 column 7\ESC[38;2;255;255;255m\
    \"
        (Bad "runtime error.")
    )

issue3_overApp = TProg
    "\
\f n = (\\x y => n + x + y);\n\
\\n\
\appMul a b = (\\n => f n (a * b));\n\
\\n\
\main = appMul 7 17 42 5 1;\n\
\"
    (Out
        "\ESC[38;2;255;0;0mError: function applied to too many arguments.\n\
    \During evaluation of:\n\
    \ 1\n\
    \Located at line 5 column 25\ESC[38;2;255;255;255m\
    \"
        (Bad "runtime error.")
    )

issue3_invPred = TProg
    "\
\f n = {\n\
\  if 1 {\n\
\    return n;\n\
\  }\n\
\  return 0;\n\
\};\n\
\\n\
\main = f 42;\n\
\"
    (Out
        "\ESC[38;2;255;0;0mError: expression `1` has invalid type 'Integer'; expected 'Bool'.\n\
    \During evaluation of:\n\
    \ if 1 {\n\
    \   return n ;\
    \ }\n\
    \Located at line 2 column 3\ESC[38;2;255;255;255m\
    \"
        (Bad "runtime error.")
    )

issue3_invEqTypes = TProg
    "\
\f a b = {\n\
\  if a == b {\n\
\    return 42;\n\
\  }\n\
\  return 0;\n\
\};\n\
\\n\
\main = f 17 ();\n\
\"
    (Out
        "\ESC[38;2;255;0;0mError: expressions `a` and `b` have invalid types 'Integer', 'Unit'; \
    \only values of the same primitive type can be equated.\n\
    \During evaluation of:\n\
    \ a == b \n\
    \Located at line 2 column 6\ESC[38;2;255;255;255m\
    \"
        (Bad "runtime error.")
    )


issue4_unaryMinus = TProg "\
\main = (-7) * (-17);\
\" (Out [] (Ok $ PInt 119))

issue5_lam = TProg
    "\
\fun = {\n\
\  f = (\\b c x => b + c - x);\n\
\  return f;\n\
\};\n\
\\n\
\main = fun 1 2 3 ;\
\"
    (Out [] (Ok $ PInt 0))

issue5_lam2 = TProg
    "\
\main = {\n\
\  var x = 42;\n\
\  var y = 3;\n\
\  var z = 17;\n\
\  var a = 7;\n\
\  f = \\ b c x => a * b + c - x + y * z;\n\
\  x += 1;\n\
\  y -= 50;\n\
\  z /= 4;\n\
\  a ^= 2;\n\
\  return f 1 2 3;\n\
\  } ;\n\
\"
    (Out [] (Ok $ PInt 57))

issue5_lam3 = TProg
    "\
\fun x y = {\n\
\  x = x * y;\n\
\  return (\\n y => x + n + y);\n\
\};\n\
\\n\
\main = fun 7 13 17 42;\n\
\"
    (Out [] (Ok $ PInt 150))

issue5_while = TProg
    "\
\fac n = {\n\
\  var result = 1;\n\
\  var i = 0;\n\
\  while i < n {\n\
\    i += 1;\n\
\    result *= i;\n\
\  }\n\
\  return result;\n\
\};\n\
\\n\
\main = fac 42;\n\
\"
    (Out [] (Ok $ PInt 1405006117752879898543142606244511569936384000000000))

issue5_ass = TProg
    "\
\fun n = {\n\
\  var y = n;\n\
\  var n = n;\n\
\  n *= 2;\n\
\  return y;\n\
\};\n\
\\n\
\main = fun 42;\n\
\"
    (Out [] (Ok $ PInt 42))

issue5_ass2 = TProg
    "\
\fun n = {\n\
\  var y = n;\n\
\  var n = n;\n\
\  n *= 2;\n\
\  return n;\n\
\};\n\
\\n\
\main = fun 42;\n\
\"
    (Out [] (Ok $ PInt 84))

issue5_immutableAss = TProg
    "\
\fun n = {\n\
\    x = n;\n\
\    x += 1;\n\
\    return x;\n\
\};\n\
\\n\
\main = fun 42;\n\
\"
    (Out
        "\ESC[38;2;255;0;0mError: cannot assign to an immutable variable `x`.\n\
    \During evaluation of:\n\
    \ x := x + 1 ; \n\
    \Located at line 3 column 5\ESC[38;2;255;255;255m\
    \"
        (Bad "runtime error.")
    )

issue5_undeclared = TProg
    "\
\fun n = {\n\
\    x = n;\n\
\    y += 1;\n\
\    return x;\n\
\};\n\
\\n\
\main = fun 42;\n\
\"
    (Out
        "\ESC[38;2;255;0;0mError: undeclared identifier `y`.\n\
    \During evaluation of:\n\
    \ y := y + 1 ; \n\
    \Located at line 3 column 5\ESC[38;2;255;255;255m\
    \"
        (Bad "runtime error.")
    )

issue5_unassignable = TProg
    "\
\fun n = {\n\
\    x = n;\n\
\    y += 1;\n\
\    return x;\n\
\};\n\
\\n\
\main = {\n\
\    fun := (\\n => n);\n\
\    return fun;\n\
\};\n\
\"
    (Out
        "\ESC[38;2;255;0;0mError: cannot assign to `fun`, which is a value of type 'Function'.\n\
    \During evaluation of:\n\
    \ fun := \\ n => n ; \n\
    \Located at line 8 column 5\ESC[38;2;255;255;255m\
    \"
        (Bad "runtime error.")
    )

issue5_unassigned = TProg
    "\
\fun = {\n\
\    var x;\n\
\    n = x;\n\
\    return n;\n\
\};\n\
\\n\
\main = fun;\n\
\"
    (Out
        "\ESC[38;2;255;0;0mError: variable `x` used before it was assigned.\n\
    \During evaluation of:\n\
    \ x \n\
    \Located at line 3 column 9\ESC[38;2;255;255;255m\
    \"
        (Bad "runtime error.")
    )

testResult :: TestProgram -> Test.HUnit.Test
testResult (TProg i (Out _ exp)) = TestCase
    (assertEqual "Expected result" exp res)
  where
    Out _ res =
        let ts = lexer i
        in  case parser ts of
                Bad e -> error e
                Ok  t -> runInterpreter t

testAll :: TestProgram -> Test.HUnit.Test
testAll (TProg i exp) = TestCase
    (assertEqual "Expected output and result" (san exp) (san res))
  where
    Out o r =
        let ts = lexer i
        in  case parser ts of
                Bad e -> error e
                Ok  t -> runInterpreter t
    res = Out (o "") r
    san (Out o r) = Out (fst $ foldr f ("", False) o) r
      where
        f c (acc, ws) = case (isSpace c, ws) of
            (True , True ) -> (acc, True)
            (True , False) -> (' ' : acc, True)
            (False, _    ) -> (c : acc, False)

tests = TestList
    [ TestLabel "issue1_sqr"          (testResult issue1_sqr)
    , TestLabel "issue1_partial"      (testResult issue1_partial)
    , TestLabel "issue1_string"       (testResult issue1_string)
    , TestLabel "issue1_bool1"        (testResult issue1_bool1)
    , TestLabel "issue1_bool2"        (testResult issue1_bool2)
    , TestLabel "issue1_bool3"        (testResult issue1_bool3)
    , TestLabel "issue1_mod"          (testResult issue1_mod)
    , TestLabel "issue1_lazy"         (testResult issue1_lazy)
    , TestLabel "issue2_fac"          (testResult issue2_fac)
    , TestLabel "issue2_lam"          (testResult issue2_lam)
    , TestLabel "issue3_divZero"      (testAll issue3_divZero)
    , TestLabel "issue3_modZero"      (testAll issue3_modZero)
    , TestLabel "issue3_invType"      (testAll issue3_invType)
    , TestLabel "issue3_noReturn"     (testAll issue3_noReturn)
    , TestLabel "issue3_overApp"      (testAll issue3_overApp)
    , TestLabel "issue3_invPred"      (testAll issue3_invPred)
    , TestLabel "issue3_invEqTypes"   (testAll issue3_invEqTypes)
    , TestLabel "issue4_unaryMinus"   (testAll issue4_unaryMinus)
    , TestLabel "issue5_lam"          (testAll issue5_lam)
    , TestLabel "issue5_lam2"         (testAll issue5_lam2)
    , TestLabel "issue5_lam3"         (testAll issue5_lam3)
    , TestLabel "issue5_while"        (testAll issue5_while)
    , TestLabel "issue5_ass"          (testAll issue5_ass)
    , TestLabel "issue5_ass2"         (testAll issue5_ass2)
    , TestLabel "issue5_immutableAss" (testAll issue5_immutableAss)
    , TestLabel "issue5_undeclared"   (testAll issue5_undeclared)
    , TestLabel "issue5_unassignable" (testAll issue5_unassignable)
    , TestLabel "issue5_unassigned"   (testAll issue5_unassigned)
    ]

main :: IO ()
main = defaultMain $ hUnitTestToTests tests
