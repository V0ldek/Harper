import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import           Data.List

import           OutputM
import           ErrM

data MyTestCase o a = FTCase (Output o a) (Output o a)
                    | OTCase (Output o a) o

getFive :: Monoid o => Output o Integer
getFive = return 5

testSuccess :: MyTestCase String Integer
testSuccess = FTCase test exp
  where
    test = do
        output "Begin\n"
        n <- getFive
        output ("Got " ++ show n ++ "\n")
        let res = n + 42
        output ("Producing result " ++ show res ++ "\n")
        return res
    exp = Out "Begin\nGot 5\nProducing result 47\n" (Ok 47)

testFailure :: MyTestCase String Integer
testFailure = FTCase (test 42) exp
  where
    test :: Integer -> Output String Integer
    test n = if n == 37
        then fail "Reached 37"
        else do
            output $ "In test " ++ show n ++ "\n"
            n' <- test (n - 1)
            output $ "Result: " ++ show n'
            return n'
    exp = Out
        "In test 42\nIn test 41\nIn test 40\nIn test 39\nIn test 38\n"
        (Bad "Reached 37")


testLazy :: MyTestCase String Integer
testLazy = OTCase test exp
  where
    test = do
        output "Begin\n"
        n <- errOut
        m <- realOut
        output "Combining\n"
        return $ n + m
    errOut = do
        output "Recursing indefinitely\n"
        inf
    inf = do
        inf
        return 42
    realOut = do
        output "Returning a value\n"
        return 17
    exp = "Begin\nRecursing indefinitely\n"

testProg :: (Eq o, Show o, Eq a, Show a) => MyTestCase [o] a -> Test.HUnit.Test
testProg (FTCase o exp) =
    TestCase (assertEqual "Expected output and result" exp o)
testProg (OTCase (Out o _) exp) =
    TestCase (assertBool "Expected output" (exp `isPrefixOf` o))

tests = TestList
    [ TestLabel "testSuccess" (testProg testSuccess)
    , TestLabel "testFailure" (testProg testFailure)
    , TestLabel "testLazy"    (testProg testLazy)
    ]

main :: IO ()
main = defaultMain $ hUnitTestToTests tests
