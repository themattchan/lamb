{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common
import Test.Tasty
import Text.Printf
import Control.Exception
import System.Exit
import Language.Fox.Types      hiding (Result)
import Paths_fox

main :: IO ()
main = do
  sc            <- initScore
  anfTests      <- readTestFile sc "tests/anf.json"
  adderTests    <- readTestFile sc "tests/adder.json"
  boaTests      <- readTestFile sc "tests/boa.json"
  dynamicTests  <- readTestFile sc "tests/dynamic.json"
  staticTests   <- readTestFile sc "tests/static.json"
  cobraTests    <- readTestFile sc "tests/cobra.json"
  diamondTests  <- readTestFile sc "tests/diamondback.json"
  eggTests      <- readTestFile sc "tests/egg.json"
  foxTests      <- readTestFile sc "tests/fox.json"
  yourTests     <- readTestFile sc "tests/yourTests.json"

  let tests = testGroup "Tests"
                [ testGroup "Normalizer"      anfTests
                , testGroup "Adder"           adderTests
                , testGroup "Boa"             boaTests
                , testGroup "Cobra"           cobraTests
                , testGroup "Dynamic"         dynamicTests
                , testGroup "static"          staticTests
                , testGroup "Diamondback"     diamondTests
                , testGroup "Egg-eater"       eggTests
                , testGroup "Fox"             foxTests
                , testGroup "Your-Tests"      yourTests
                ]
  defaultMain tests `catch` (\(e :: ExitCode) -> do
    (n, tot) <- getTotal sc
    putStrLn ("OVERALL SCORE = " ++ show n ++ " / "++ show tot)
    throwIO e)


readTestFile      :: Score -> FilePath -> IO [TestTree]
readTestFile sc f = getDataFileName f >>= readTests sc

readTests     :: Score -> FilePath -> IO [TestTree]
readTests sc f = map (createTestTree sc) <$> parseTestFile f
