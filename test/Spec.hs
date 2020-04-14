import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Harper.Lexer
import Harper.Parser
import Harper.Engine
import Harper.Engine.Object
import ErrM

data TestProgram = TProg String Object

lexer  = myLexer
parser = pProgram

issue1_sqr = TProg "\
\sqr n = n * n;\n\

\main = sqr 42;" 
    (PInt 1764)

issue1_partial = TProg "\
\f a b c d = (c ^ d / a) * 123 + b;\n\

\fun n = f n;\n\

\main = (fun 2) 420 6 4;"
    (PInt 80124)

issue1_string = TProg "\
\main = \"abcd\";"
    (PStr "abcd")

issue1_bool1 = TProg "\
\f n = 2 * n;\n\

\main = (f 42 == 84) and (f 2 <= f 4) and (f 2 < f 5) and (true and not false);"
    (PBool True)

issue1_bool2 = TProg "\
\main = true or false;"
    (PBool True)

issue1_bool3 = TProg "\
\main = true and false;"
    (PBool False)

issue1_mod = TProg "\
\f n = 2 * n;\n\

\main = f 5 mod 3 == 1 and f 5 mod 3 != 2 and f 5 mod 3 > 0 and f 5 mod 3 >= 1;"
    (PBool True)

issue1_lazy = TProg "\
\fun n = fun (n + 1);\n\

\main = true or (fun 42);"
    (PBool True)

issue2_fac = TProg "\
\fac n = {\
\  if (n <= 0) {\
\    return 1;\
\  }\
\  else {\
\    return fac (n - 1) * n;\
\  }\
\};\
\\n\
\main = fac 42;\
\"
    (PInt 1405006117752879898543142606244511569936384000000000)

issue2_lam = TProg "\
\f n = {\
\  if n mod 2 == 0 {\
\    return (\val x val y => x * y * n);\
\  }\
\  else if n == 37 {\
\    return (\val x val y => 42);\
\  }\
\  else {\
\    return (\val x val y => x + y + n);\
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
    (PBool True)

testProg :: TestProgram -> Test.HUnit.Test
testProg (TProg i o) = TestCase (assertEqual "Expected output" o run)
    where run = let ts = lexer i
                 in case parser ts of
                     Bad e -> error e
                     Ok  t -> fst $ runInterpreter t

tests = TestList [TestLabel "issue1_sqr" (testProg issue1_sqr),
                  TestLabel "issue1_partial" (testProg issue1_partial),
                  TestLabel "issue1_string" (testProg issue1_string),
                  TestLabel "issue1_bool1" (testProg issue1_bool1),
                  TestLabel "issue1_bool2" (testProg issue1_bool2),
                  TestLabel "issue1_bool3" (testProg issue1_bool3),
                  TestLabel "issue1_mod" (testProg issue1_mod),
                  TestLabel "issue1_lazy" (testProg issue1_lazy)
                 ]

main :: IO ()
main = defaultMain $ hUnitTestToTests tests
