{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- | This module contains the code for converting an `Expr` to a "A-Normal" form.
--------------------------------------------------------------------------------
module Language.Lamb.Checker
  ( -- * Top-level Static Checker
    check

    -- * Error Constructors
  , errUnboundVar
  , errUnboundFun
  ) where

import           Control.Exception
import qualified Data.List          as L
import           Text.Printf        (printf)
import           Language.Lamb.Types
import           Language.Lamb.Utils

--------------------------------------------------------------------------------
check :: BareProgram -> BareProgram
--------------------------------------------------------------------------------
check p = case wellFormed p of
            [] -> p
            es -> throw es

-- | Map from function name to arity
type FunEnv = Env

--------------------------------------------------------------------------------
-- | `wellFormed p` returns the list of errors for a program `p`
--------------------------------------------------------------------------------
wellFormed :: BareProgram -> [UserError]
--------------------------------------------------------------------------------
wellFormed (Prog ds e) = duplicateFunErrors ds
                      ++ concatMap (wellFormedD fEnv) ds
                      ++ wellFormedE fEnv emptyEnv e
  where
    fEnv               = fromListEnv [(bindId f, length xs) | Decl f xs _ _ <- ds]

--------------------------------------------------------------------------------
-- | `wellFormedD fEnv vEnv d` returns the list of errors for a func-decl `d`
--------------------------------------------------------------------------------
wellFormedD :: FunEnv -> BareDecl -> [UserError]
wellFormedD fEnv (Decl _ xs e _) = duplicateParamErrors xs
                                ++ wellFormedE fEnv vEnv e
  where
    vEnv                         = addsEnv xs emptyEnv

--------------------------------------------------------------------------------
-- | `wellFormedE vEnv e` returns the list of errors for an expression `e`
--------------------------------------------------------------------------------
wellFormedE :: FunEnv -> Env -> Bare -> [UserError]
wellFormedE fEnv = go
  where
    gos                       = concatMap . go
    go _    (Boolean {})      = []
    go _    (Number  n     l) = largeNumberErrors      n l
    go vEnv (Id      x     l) = unboundVarErrors  vEnv x l
    go vEnv (Prim1 _ e     _) = go  vEnv e
    go vEnv (Prim2 _ e1 e2 _) = gos vEnv [e1, e2]
    go vEnv (If   e1 e2 e3 _) = gos vEnv [e1, e2, e3]
    go vEnv (Let x e1 e2   _) = duplicateBindErrors vEnv x
                             ++ go vEnv e1
                             ++ go (addEnv x vEnv) e2
    go vEnv (Tuple es      _) = error "TBD:wellFormed:Tuple"
    go vEnv (GetItem e1 e2 _) = error "TBD:wellFormed:GetItem"
    go vEnv (App f es      l) = callArityErrors fEnv f es l
                             ++ unboundFunErrors fEnv f l
                             ++ gos vEnv es

addsEnv :: [BareBind] -> Env -> Env
addsEnv xs env = L.foldl' (\env x -> addEnv x env) env xs

--------------------------------------------------------------------------------
-- | Error Checkers: In each case, return an empty list if no errors.
--------------------------------------------------------------------------------
duplicateFunErrors :: [BareDecl] -> [UserError]
duplicateFunErrors ds
  = map errDupFun
  . concat
  . dupBy (bindId . fName)
  $ ds

duplicateParamErrors :: [BareBind] -> [UserError]
duplicateParamErrors xs
  = map errDupParam
  . map head
  . dupBy bindId
  $ xs

duplicateBindErrors :: Env -> BareBind -> [UserError]
duplicateBindErrors vEnv x
  = condError (memberEnv (bindId x) vEnv) (errDupBind x)

largeNumberErrors :: Integer -> SourceSpan -> [UserError]
largeNumberErrors n l
  = condError (maxInt < abs n) (errLargeNum l n)

maxInt :: Integer
maxInt = 1073741824

unboundVarErrors :: Env -> Id -> SourceSpan -> [UserError]
unboundVarErrors vEnv x l
  = condError (not (memberEnv x vEnv)) (errUnboundVar l x)

unboundFunErrors :: FunEnv -> Id -> SourceSpan -> [UserError]
unboundFunErrors fEnv f l
  = condError (not (memberEnv f fEnv)) (errUnboundFun l f)


callArityErrors :: FunEnv -> Id -> [Bare] -> SourceSpan -> [UserError]
callArityErrors fEnv f es l
  = case lookupEnv f fEnv of
      Just n  -> condError (n /= length es) (errCallArity l f)
      Nothing -> []
--------------------------------------------------------------------------------
-- | Error Constructors
--------------------------------------------------------------------------------
condError :: Bool -> UserError -> [UserError]
condError True  e = [e]
condError False _ = []

errDupFun d       = mkError (printf "duplicate function '%s'" (pprint f))    (sourceSpan f) where f = fName d
errDupParam     x = mkError (printf "Duplicate parameter '%s'" (bindId x)) (sourceSpan x)
errDupBind      x = mkError (printf "Shadow binding '%s'" (bindId x))      (sourceSpan x)
errLargeNum   l n = mkError (printf "Number '%d' is too large" n) l
errUnboundVar l x = mkError (printf "Unbound variable '%s'" x) l
errUnboundFun l f = mkError (printf "Function '%s' is not defined" f) l
errCallArity  l f = mkError (printf "Wrong arity of arguments at call of %s" f) l
