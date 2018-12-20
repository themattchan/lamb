{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable,
ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
module Language.Lamb.AST where

import Language.Lamb.UX (SourceSpan(..))
import Data.List
import Data.Functor.Const
import Data.Word
import Data.Int

data T0 f = T0 (f (T0 f))

eraseAnn :: Functor f => T ann f -> T0 f
eraseAnn = T0 . fmap eraseAnn . out

-- cofree comonad
data T ann f  = T ann (f (T ann f))

getAnn :: T ann f -> ann
getAnn (T a _) = a

setAnn :: ann -> T ann f -> T ann f
setAnn a (T _ f) = T a f

out :: T ann f -> f (T ann f)
out (T _ f) = f

inn' :: ann -> f (T ann f) -> T ann f
inn' ann f = T ann f

-- TODO mendler style cata

foldT :: (Functor f) => (f x -> x) -> T ann f -> x
foldT alg = alg . fmap (foldT alg) . out

foldTA :: (Functor f) => (ann -> f x -> x) -> T ann f -> x
foldTA alg (T ann f) = alg ann (fmap (foldTA alg) f)

foldTAM :: (Traversable f, Monad m) => (ann -> f x -> m x) -> T ann f -> m x
foldTAM alg (T ann f) = alg ann =<< traverse (foldTAM alg) f

{-
-- traverse :: Applicative m, Traversable f => (a -> m b) -> f a -> m (f b)
mapAnnTM :: (Monad m, Traversable f)
         => (T ann1 f -> m ann2)
         -> T ann1 f
         -> m (T ann2 f)
mapAnnTM f = foldTAM f' -- inn' <$> f t <*> traverse f' (out t)
  where
    f' ann fx = f (T ann --?????
--    f' :: T ann1 f -> m (T ann2 f)
--    f' tInner = (`setAnn` tInner) <$> f tInner
-}
--------------------------------------------------------------------------------

-- abstract over binders: parse into named bndrs, then transform into nameless
-- repr

-- an expr is a lambda calculus with sums and products.
data ExpF bnd lit e
  = Lit lit -- literals -- split into mylit and backend lit?
-- some variable
  | Bnd bnd

  -- maybe use HOAS??
  -- | Fun (bnd -> e)
  | Fun bnd e

  -- nonrecursive let*
  | Let [(bnd, e)] e

  | App e e

  -- TODO: consider anonymous sums and products
  -- | Sum (bnd, e) (bnd, e)
  -- | Prd (bnd, e) (bnd, e)
-- this shall just be: App (App ("elim") e) e
--  | Elim e e
  deriving (Functor, Foldable, Traversable, Show)

showExpF :: (Show bnd, Show lit) => ExpF bnd lit String -> String
showExpF (Lit l) = "(Lit " <> show l <> ")"
showExpF (Bnd b) = "(Bnd " <> show b <> ")"
showExpF (Fun b e) = "(fun " <> show b <> " => " <> e <> ")"
showExpF (Let bs e) = "(let " <> intercalate ", " [show x<>" = "<>y| (x,y)<-bs] <> " in " <> e <> ")"
showExpF (App e1 e2) = "(App " <> e1 <>" "<>e2<> ")"

funs :: [(bnd, ann)] -> T ann (ExpF bnd lit) -> T ann (ExpF bnd lit)
funs args e = foldr (\(arg, ann) e' -> T ann (Fun arg e')) e args
types = funs

apps :: (ann -> ann -> ann)
     -> T ann (ExpF bnd lit)
     -> [T ann (ExpF bnd lit)]
     -> T ann (ExpF bnd lit)
apps (<>) op args = foldr (\x a -> T (getAnn a <> getAnn x) (App a x)) op args

-- primitive C types for now.
data ELit
  = CUInt8 Word8
  | CUInt16 Word16
  | CUInt32 Word32
  | CUInt64 Word64

  | CInt8 Int8
  | CInt16 Int16
  | CInt32 Int32
  | CInt64 Int64

  | CBool Bool
-- todo: heap types, strings, etc
  deriving (Show, Eq)

-- the duplication here is rather unfortunate.
data TLit
  = CUInt8t
  | CUInt16t
  | CUInt32t
  | CUInt64t

  | CInt8t
  | CInt16t
  | CInt32t
  | CInt64t
  deriving (Show, Eq)

type Exp bnd ann = T ann (ExpF bnd ELit)

instance (Show bnd) => Show (Exp bnd ann) where show = foldT showExpF

-- FIXME want a separate type for DT definitions
type Typ bnd = T () (ExpF bnd TLit)
instance (Show bnd) => Show (Typ bnd) where show = foldT showExpF


-- A decl is either a named supercombinator expression
-- or a datatype.
-- ?? can you remove ann from here ??
data DeclF bnd ann e
  = Sc bnd (Exp bnd ann)
  | Dt bnd (Typ bnd)
  deriving (Show,Functor)
type Decl bnd ann = T ann (DeclF bnd ann)
instance (Show bnd) => Show (Decl bnd ann) where show = foldT show

-- A module is a collection of declarations.
data ModF bnd ann e
  = Mod bnd [Decl bnd ann]
  deriving (Show, Functor)
type Mod bnd ann = T ann (ModF bnd ann)
instance (Show bnd) => Show (Mod bnd ann) where
--  show = foldT show
  show (T _ (Mod bnd s)) = "module " <> show bnd <> " where" <> show s

type RawName = String
type Ann = ()

-- Actual types used

-- TODO use row-types for ann
type LExp ann = Exp RawName ann
type LTyp = Typ RawName
type LDecl ann = Decl RawName ann
type LMod ann = Mod RawName ann

-- type AExp = Exp Name (SourceSpan,Int)
-- type ATyp = Typ Name
-- type ADecl = Decl Name (SourceSpan,Int)
-- type AMod = Mod Name (SourceSpan,Int)

data Name = MyName
