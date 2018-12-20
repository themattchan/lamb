{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Lamb.Utils
  ( module Language.Lamb.Utils
  , module Control.Applicative
  , module Control.Arrow
  , module Control.Monad
  , module Data.Bifunctor
  , module Data.Bool
  , module Data.Char
  , module Data.Either
  , module Data.Function
  , module Data.Foldable
--  , module Data.Semigroup
  , module Data.Tuple
  , module Data.Maybe
  , module Data.Monoid
  , module Data.List
  , module Data.Ord
  )
where

import Control.Applicative
import Control.Arrow ((***), (&&&), (>>>), (<<<))
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import qualified Data.Array as A
import Data.Bifunctor
import Data.Bits
import Data.Bool (bool)
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function (on, (&))
import Data.Functor.Identity
import Data.List hiding (groupBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import Data.Tuple (swap)
import Debug.Trace (trace)
import Numeric (showHex, showIntAtBase)
import System.Console.CmdArgs.Verbosity (whenLoud)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import System.Timeout
import Text.Printf
--import Data.Semigroup
--import qualified Text.Regex.TDFA as RE
--import qualified Text.Regex.TDFA.String as RE

--------------------------------------------------------------------------------
(>->) :: (a -> Either e b) -> (b -> c) -> a -> Either e c
--------------------------------------------------------------------------------
f >-> g = f >=> safe g
  where
    safe :: (a -> b) -> a -> Either c b
    safe h x = Right (h x)

groupBy :: (Ord k) => (a -> k) -> [a] -> [(k, [a])]
groupBy f = M.toList . foldl' (\m x -> inserts (f x) x m) M.empty

inserts :: (Ord k) => k -> v -> M.Map k [v] -> M.Map k [v]
inserts k v m = M.insert k (v : M.findWithDefault [] k m) m

dupBy :: (Ord k) => (a -> k) -> [a] -> [[a]]
dupBy f xs = [ xs' | (_, xs') <- groupBy f xs, 2 <= length xs' ]

trim :: String -> String
trim = f . f  where f = reverse . dropWhile isSpace

trimEnd :: String -> String
trimEnd = reverse . dropWhile isSpace . reverse

executeShellCommand :: FilePath -> String -> Int -> IO ExitCode
executeShellCommand logF cmd n = fromMaybe (ExitFailure 100) <$> body
  where
    body = timeout n . withFile logF AppendMode $ \h -> do
             let p       = (shell cmd) {std_out = UseHandle h, std_err = UseHandle h}
             (_,_,_,ph) <- createProcess p
             waitForProcess ph

data Phase = Start | Stop deriving (Show)

phase :: Phase -> String -> IO ()
phase p msg = putStrLn $ printf "**** %s : %s **************************************" (show p) msg

writeLoud :: String -> IO ()
writeLoud s = whenLoud $ putStrLn s >> hFlush stdout

ensurePath :: FilePath -> IO ()
ensurePath = createDirectoryIfMissing True . takeDirectory

safeReadFile :: FilePath -> IO (Either String String)
safeReadFile f = (Right <$> readFile f) `catch` handleIO f

handleIO :: FilePath -> IOException -> IO (Either String a)
handleIO f e = return . Left $ "Warning: Couldn't open " <> f <> ": " <> show e

traceShow :: (Show a) => String -> a -> a
traceShow msg x = trace (printf "TRACE: %s = %s" msg (show x)) x

safeHead :: a -> [a] -> a
safeHead def []    = def
safeHead _   (x:_) = x

getRange :: Int -> Int -> [a] -> [a]
getRange i1 i2
  = take (i2 - i1 + 1)
  . drop (i1 - 1)


class Fresh s where
  genzero :: s
  gennext :: s -> s

instance Fresh Int where
  genzero = 0
  gennext = (+1)

newtype GensymT s m a = GensymT { unGensymT :: StateT s m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

type GensymM a = GensymT Int Identity a
-- instance Monad m => MonadState s (GensymT s m) where
--   state = GensymT . state

-- class Gensym s m where
--   gensym :: m s

class (Fresh s, Monad m) => Gensym s m | m -> s where
  gensym :: m s

instance  (Fresh s, Monad m) => Gensym s (GensymT s m) where
  gensym = GensymT $ do
    s <- get
    modify gennext
    return s

runGensymT :: (Fresh s, Monad m) => GensymT s m a -> m a
runGensymT = flip evalStateT genzero . unGensymT
