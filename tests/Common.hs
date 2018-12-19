{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Common where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf
import Control.Exception
import System.FilePath                  ((</>), (<.>))
import System.IO                        (withFile)
import System.Exit
import Data.List                        (isInfixOf)
import Data.Char (toLower)
import Data.IORef
import Language.Fox.Utils
import Language.Fox.Types      hiding (Result)
import Language.Fox.Parser
import Language.Fox.Normalizer

import Debug.Trace                    (trace)
import Data.Aeson          hiding     (Result)
import Data.Aeson.Types               (typeMismatch)
import qualified Data.Text            as T
import qualified Data.HashMap.Lazy    as M
import qualified Data.ByteString.Lazy as B


overflowError  = Left "Error: arithmetic overflow"
rLines         = Right . unlines
dynamicError t = Left ("Error: expected a " ++ pprint t)
staticError    = Left

--------------------------------------------------------------------------------
exec :: Text -> IO ()
--------------------------------------------------------------------------------
exec s = do
  res <- run "ghci_test" Nothing (Code s)
  case res of
    Left e  -> putStrLn ("Error: " ++ e)
    Right r -> putStrLn ("Result: " ++ r)

--------------------------------------------------------------------------------
run :: FilePath -> Maybe Int -> Program' -> IO Result
--------------------------------------------------------------------------------
run name heapMb pgm = do
  _ <- generateSource name pgm                 -- generate source file
  r <- executeShellCommand logF cmd timeLimit  -- compile & run
  readResult resF logF r
  where
    cmd    = printf "make %s %s"  resF heapSz
    resF   = dirExt "output" name VRes
    logF   = dirExt "output" name Log
    heapSz = maybe "" (("HEAP=" ++) . show) heapMb

-- | `timeLimit` for each test is 15 seconds
timeLimit :: Int
timeLimit = 15 * (10 ^ 6)

--------------------------------------------------------------------------------
generateSource :: FilePath -> Program' -> IO ()
--------------------------------------------------------------------------------
generateSource _    File       = return ()
generateSource name (Code pgm) = writeFile srcF pgm
  where
    srcF                       = dirExt "input"  name Src

--------------------------------------------------------------------------------
readResult :: FilePath -> FilePath -> ExitCode -> IO Result
--------------------------------------------------------------------------------
readResult resF _     ExitSuccess      = Right <$> readFile resF
readResult _    _    (ExitFailure 100) = Left  <$> return "TIMEOUT!"
readResult _    logF (ExitFailure _  ) = Left  <$> readFile logF

dirExt :: FilePath -> FilePath -> Ext -> FilePath
dirExt dir name e = "tests" </> dir </> name `ext` e

--------------------------------------------------------------------------------
-- | A test program is either a filename or a text representation of source
--------------------------------------------------------------------------------
data Program' = File | Code Text deriving Show
type Result  = Either Text Text

--------------------------------------------------------------------------------
-- | Construct a single compiler test case from a `Program`
------------------------------------------------------------------------------
mkTest :: Maybe Int -> Score -> String -> Program' -> Result -> TestTree
mkTest heapMb sc name pgm = mkTest' sc 1 name (run name heapMb pgm)

anfTest sc name (Code inS) expS = mkTest' sc 1 name (return $ anfRun inS) expS
anfTest sc name (File) expS = mkTest' sc 1 name (anfRun <$> readFile srcF) expS
    where srcF = dirExt "input"  name Src

anfRun :: Text -> Result
anfRun = Right . pprint . anormal . parse ""

mkTest' :: Score -> Int -> String -> IO Result -> Result -> TestTree
mkTest' sc n name act expect = testCase name $ do
  updateTotal sc n
  res <- act
  check sc n res expect

--------------------------------------------------------------------------------
scoreTest' :: (Show b, Eq b) => Score -> (a -> b, a, b, Int, String) -> TestTree
--------------------------------------------------------------------------------
scoreTest' sc (f, x, expR, points, name) =
  testCase name $ do
    updateTotal sc points
    if f x == expR
      then updateCurrent sc points
      else assertFailure "Wrong Result"

-- check :: Result -> Result -> TestTree
check sc n (Right resV) (Right expectV)
  | trim expectV == trim resV          = updateCurrent sc n
  | otherwise                          = assertFailure "Wrong result"
check sc n (Left resE)  (Left  expectE)
  | matchError expectE resE            = updateCurrent sc n
  | otherwise                          = assertFailure "Wrong error"

check _ _ (Left resE)  (Right expectV) = assertEqual "Unexpected error"   ("Value " ++ expectV) ("Error " ++ resE)
check _ _ (Right resV) (Left  expectE) = assertEqual "Unexpected result"  ("Error " ++ expectE) ("Value " ++ resV)

matchError expectE resE = tx expectE `isInfixOf` tx resE
  where
      tx = map toLower
--------------------------------------------------------------------------------
type Score = IORef (Int, Int)
--------------------------------------------------------------------------------

getTotal :: Score -> IO (Int, Int)
getTotal = readIORef

updateTotal :: Score -> Int -> IO ()
updateTotal sc n = modifyIORef sc (\(x, y) -> (x, y + n))

updateCurrent :: Score -> Int -> IO ()
updateCurrent sc n = modifyIORef sc (\(x, y) -> (x + n, y))

initScore :: IO Score
initScore = newIORef (0, 0)

--------------------------------------------------------------------------------
-- | Tests
--------------------------------------------------------------------------------

-- { name   : string
-- , code   : "file" | string
-- , result : { value : string } | { failure : string }
-- }

data TestResult = Value   String
                | Failure String
                deriving (Show)

data Test = Test { testName   :: String
                 , testCode   :: Program'
                 , testResult :: TestResult
                 , testAnf    :: Bool
                 , testHeap   :: Int
                 }
          deriving (Show)

instance FromJSON Program' where
  parseJSON = withText "\"file\" or String" (return . helper . T.unpack)
    where helper s = if s == "file" then File else Code s

instance FromJSON TestResult where
  parseJSON = withObject expected $ \obj -> helper (M.toList obj) obj
    where expected                    = "{ \"value\": String } or { \"failure\" : String }"
          helper [("value", val)]   _ = withText "String" (return . Value   . T.unpack) val
          helper [("failure", err)] _ = withText "String" (return . Failure . T.unpack) err
          helper _                obj = typeMismatch expected (Object obj)

instance FromJSON Test where
  parseJSON = withObject expected p
    where p o      = if length (M.keys o) == 3 || length (M.keys o) == 4
                     then Test <$> o .: "name"
                               <*> o .: "code"
                               <*> o .: "result"
                               <*> o .:? "anf"  .!= False
                               <*> o .:? "heap" .!= 0

                     else typeMismatch expected (Object o)
          expected = "{ \"name\" : ..., \"code\" : ..., \"result\" : ..., \"anf\" : \"False\"}"

createTestTree sc Test{..} = mk sc testName testCode res
  where
    res                    =  case testResult of
                                Value   s -> Right s
                                Failure s -> Left  s
    mk  | testAnf          = anfTest
        | otherwise        = mkTest (heapSize testHeap)

heapSize :: Int -> Maybe Int
heapSize n
  | 0 < n     = Just n
  | otherwise = Nothing

parseTestFile  :: FilePath -> IO [Test]
parseTestFile f = do b <- B.readFile f
                     case eitherDecode b of
                       Left err  -> fail $ "!!! ERROR IN TEST FILE" ++ f ++ " : " ++ err
                       Right res -> return res
