import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import           System.Directory
import           System.FilePath

import           Control.Monad
import           Data.Char
import           Data.List
import qualified Data.Set                      as Set
import           Harper.Lexer
import           Harper.Parser
import           Harper.Interpreter
import           Harper.Interpreter.Core
import           Harper.TypeChecker
import           Harper.Printer
import           ErrM
import           OutputM

data TestProgram = TProg String String

lexer = myLexer
parser = pProgram

testOutput :: TestProgram -> Test.HUnit.Test
testOutput (TProg prog out) = TestCase
    (assertEqual "Expected output" (normalize out) (normalize run))
  where
    run =
        let ts = lexer prog
        in
            case parser ts of
                Bad e -> error e
                Ok t ->
                    let Out out res = runTypeChecker t
                    in
                        case res of
                            Ok (t', tenv) ->
                                let Out out' res' = runInterpreter t' tenv
                                in
                                    case res' of
                                        Ok v ->
                                            out' (out "")
                                                ++ "\nExecution ended with value:\n"
                                                ++ v ""
                                        Bad s ->
                                            out' (out "")
                                                ++ "\nExecution terminated with an error: "
                                                ++ s
                            Bad s ->
                                out ""
                                    ++ "\nExecution terminated with an error: "
                                    ++ s


normalize s =
    fst $ foldr normWs ("", False) $ dropWhile isSpace $ rmOccs white $ rmOccs
        red
        s
  where
    normWs c (acc, ws) = case (isSpace c, ws) of
        (True , True ) -> (acc, True)
        (True , False) -> (' ' : acc, True)
        (False, _    ) -> (c : acc, False)
    rmOccs w "" = ""
    rmOccs w s@(c : cs) | w `isPrefixOf` s = rmOccs w (drop (length w) s)
                        | otherwise        = c : rmOccs w cs
    red   = "\ESC[38;2;255;0;0m"
    white = "\ESC[38;2;255;255;255m"

goodDirectory = "./test/Harper/Tests/Good"
badDirectory = "./test/Harper/Tests/Bad"

main :: IO ()
main = do
    goodTests <- makeTestsFromFiles goodDirectory
    badTests  <- makeTestsFromFiles badDirectory
    let tests = sortOn (\(TestLabel n _) -> n) (goodTests ++ badTests)
    defaultMain $ hUnitTestToTests (TestList tests)
  where
    makeTestsFromFiles dir = do
        fileList <- getDirectoryContents dir
        let files    = Set.fromList fileList
            harFiles = Set.filter (".har" `isExtensionOf`) files
            harOutErr =
                Set.map (\har -> (har, lookupWithExt ".out" har files)) harFiles
        mapM pairToTest (Set.toList harOutErr)
      where
        lookupWithExt ext f = setLookup (replaceExtensions f ext)
        pairToTest (har, Just out) = do
            prog <- readFile (dir </> har)
            res  <- readFile (dir </> out)
            let Just testName = stripExtension ".har" har
            return $ TestLabel testName (testOutput $ TProg prog res)

setLookup :: Ord a => a -> Set.Set a -> Maybe a
setLookup a s = if Set.member a s then Just a else Nothing
