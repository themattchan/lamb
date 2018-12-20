{-# LANGUAGE DeriveAnyClass, DeriveFoldable, KindSignatures, TypeFamilies, PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Language.Lamb.AST where
import Language.Lamb.UX (SourceSpan(..))

import Control.Comonad.Cofree
import Data.Functor.Base
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Functor.Classes
import Data.Functor.Const
import Data.Word
import Data.Int

type T ann f = Cofree f ann

pattern T ann f = ann :< f

{-
-- cofree comonad
data T ann f  = T ann (f (T ann f))

instance (Show ann, Show1 f) => Show (T ann f) where
  showsPrec i (T ann f) = (("T "++) . showsPrec i ann) . ((" " ++) . showsPrec1 i f)

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
-}
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
data Exp bnd lit
  = Lit lit -- literals -- split into mylit and backend lit?
-- some variable
  | Bnd bnd

  -- maybe use HOAS??
  -- | Fun (bnd -> e)
  | Fun bnd (Exp bnd lit)

  -- nonrecursive let*
  | Let [(bnd, Exp bnd lit)] (Exp bnd lit)

  | App (Exp bnd lit) (Exp bnd lit)
  -- TODO: consider anonymous sums and products
  -- | Sum (bnd, e) (bnd, e)
  -- | Prd (bnd, e) (bnd, e)
-- this shall just be: App (App ("elim") e) e
--  | Elim e e
  deriving (Functor, Foldable, Traversable, Show)

-- instance (Show bnd, Show lit) => Show1 (ExpF bnd lit) where
--   liftShowsPrec sp sl i (Lit l) = ("Lit " ++) . showsPrec i l
--   liftShowsPrec sp sl i (Bnd b) = (("(Bnd " ++ show b ++ ")") <>)
--   liftShowsPrec sp sl i (Fun b e) = (("(fun " ++ show b ++ " => ") <>) . sp i e . (")"<>)
--   liftShowsPrec sp sl i (Let l e) = ("Let " ++) . f l . (" in " <>) . sp i e
--     where
--       f = foldl (\acc (b,e) -> ((show b <> " = ") <> ) . sp i e . (",\n " <>) . acc) id
--   liftShowsPrec sp sl i (App x y) = ("App(" ++) . sp i x . (", " <>) . sp i y . (")" <>)

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


-- A decl is either a named supercombinator expression
-- or a datatype.
-- ?? can you remove ann from here ??
data Decl bnd
  = Sc bnd (Exp bnd ELit)
  | Dt bnd (Exp bnd TLit)
  deriving Show

-- instance (Show bnd, Show ann) => Show1 (DeclF bnd ann) where
--   liftShowsPrec sp sl i (Sc b e) = (("Sc " <> show b <> " = ") <>) . showsPrec i e . ("\n"<>)
--   liftShowsPrec sp sl i (Dt b e) = (("type " <> show b <> " = ") <>) . showsPrec i e. ("\n"<>)

--type Decl bnd ann = T ann (DeclF bnd ann)

-- A module is a collection of declarations.
data Mod bnd
  = Mod bnd [Decl bnd]
  deriving Show

makeBaseFunctor ''Exp
makeBaseFunctor ''Decl
makeBaseFunctor ''Mod


--type Exp bnd ann = T ann (ExpF bnd ELit)
type LambExp bnd ann = Cofree (ExpF bnd ELit) ann

funs :: [(bnd, ann)] -> T ann (ExpF bnd lit) -> T ann (ExpF bnd lit)
funs args e = foldr (\(arg, ann) e' -> T ann (Fun arg e')) e args
types = funs

-- FIXME want a separate type for DT definitions
--type Typ bnd = T () (ExpF bnd TLit)

-- types dont have ann
type LambTyp bnd = Mu (ExpF bnd TLit)

type LambDecl bnd ann = Cofree (DeclF bnd) ann
type LambMod bnd ann = Cofree (ModF bnd) ann

-- instance (Show bnd, Show ann) => Show1 (ModF bnd ann) where
--   liftShowsPrec sp sl i (Mod b e) = (("Module " <> show b <> " where \n") <>) . showList e

--type Mod bnd ann = T ann (ModF bnd ann)

type Name = String
type Ann = ()

-- Actual types used

-- TODO use row-types for ann
type LExp ann = LambExp Name ann
type LTyp = LambTyp Name
type LDecl ann = LambDecl Name ann
type LMod ann = LambMod Name ann

-- type AExp = Exp Name (SourceSpan,Int)
-- type ATyp = Typ Name
-- type ADecl = Decl Name (SourceSpan,Int)
-- type AMod = Mod Name (SourceSpan,Int)
