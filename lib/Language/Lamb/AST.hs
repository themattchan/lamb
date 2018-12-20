{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, ScopedTypeVariables #-}
module Language.Lamb.AST where
import Language.Lamb.UX (SourceSpan(..))

import Data.Functor.Const
import Data.Word
import Data.Int

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
  deriving (Functor, Foldable, Traversable)

funs :: [(bnd, ann)] -> T ann (ExpF bnd lit) -> T ann (ExpF bnd lit)
funs args e = foldr (\(arg, ann) e' -> T ann (Fun arg e')) e args
types = funs

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

-- FIXME want a separate type for DT definitions
type Typ bnd = T () (ExpF bnd TLit)


-- A decl is either a named supercombinator expression
-- or a datatype.
-- ?? can you remove ann from here ??
data DeclF bnd ann e
  = Sc bnd (Exp bnd ann)
  | Dt bnd (Typ bnd)

type Decl bnd ann = T ann (DeclF bnd ann)

-- A module is a collection of declarations.
data ModF bnd ann e
  = Mod bnd [Decl bnd ann]

type Mod bnd ann = T ann (ModF bnd ann)

type Name = String
type Ann = ()

-- Actual types used

-- TODO use row-types for ann
type LExp ann = Exp Name ann
type LTyp = Typ Name
type LDecl ann = Decl Name ann
type LMod ann = Mod Name ann

-- type AExp = Exp Name (SourceSpan,Int)
-- type ATyp = Typ Name
-- type ADecl = Decl Name (SourceSpan,Int)
-- type AMod = Mod Name (SourceSpan,Int)
