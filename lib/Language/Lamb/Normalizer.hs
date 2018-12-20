--------------------------------------------------------------------------------
-- | This module contains the code for converting an `Expr` to a "A-Normal" form.
--------------------------------------------------------------------------------

module Language.Lamb.Normalizer ( anormal ) where

--import           Language.Lamb.Types
import Language.Lamb.AST
import Language.Lamb.Utils


{-
--type Binds a = [(Bind a,a (AnfExpr a, a))]

--------------------------------------------------------------------------------
-- | Convert an Expr into A-Normal Form
--------------------------------------------------------------------------------
anormal :: Program a -> AnfProgram a
--------------------------------------------------------------------------------
anormal (Prog ds e) = Prog (anfDecl <$> ds) (anfExpr e)
  where
    anfDecl d       = d { fBody = anfExpr (fBody d) }
    anfExpr e       = snd (anf 0 e)

--------------------------------------------------------------------------------
-- | `anf i e` takes as input a "start" counter `i` and expression `e` and
--   returns an output `(i', e')` where
--   * `i'` is the output counter (i.e. i' - i) anf-variables were generated,
--   * `e'` is equivalent to `e` but is in A-Normal Form.
--------------------------------------------------------------------------------
anfE :: LExp -> GensymM AExp
--------------------------------------------------------------------------------
anf i (Number n l)      = (i, Number n l)

anf i (Boolean b l)     = (i, Boolean b l)

anf i (Id     x l)      = (i, Id     x l)

anf i (Let x e b l)     = (i'', Let x e' b' l)
  where
    (i',  e')           = anf i e
    (i'', b')           = anf i' b

anf i (Prim1 o e l)     = (i', stitch bs  (Prim1 o ae l))
  where
    (i', bs, ae)        = imm i e

anf i (Prim2 o e1 e2 l) = (i'', stitch bs'' (Prim2 o e1' e2' l))
  where
    bs''                = bs' ++ bs
    (i' , bs , e1')     = imm i  e1
    (i'', bs', e2')     = imm i' e2

anf i (If c e1 e2 l)    = (i''', stitch bs  (If c' e1' e2' l))
  where
    (i'  , bs, c')      = imm i   c
    (i'' ,     e1')     = anf i'  e1
    (i''',     e2')     = anf i'' e2

anf i (Tuple es l)      = error "TBD:anf:Tuple"

anf i (GetItem e1 e2 l) = error "TBD:anf:GetItem"

anf i (App f es l)      = (i', stitch bs (App f es' l))
  where
    (i', bs, es')       = imms i es

--------------------------------------------------------------------------------
-- | `stitch bs e` takes a "context" `bs` which is a list of temp-vars and their
--   definitions, and an expression `e` that uses the temp-vars in `bs` and glues
--   them together into a `Let` expression.
--------------------------------------------------------------------------------
stitch :: Binds a -> AnfExpr a -> AnfExpr a
--------------------------------------------------------------------------------
stitch bs e = bindsExpr [ (x, e) | (x, (e, _)) <- reverse bs] e (getLabel e)

--------------------------------------------------------------------------------
-- | `imms i es` takes as input a "start" counter `i` and expressions `es`, and
--   and returns an output `(i', bs, es')` where
--   * `i'` is the output counter (i.e. i'- i) anf-variables were generated
--   * `bs` are the temporary binders needed to convert `es` to immediate vals
--   * `es'` are the immediate values  equivalent to es
--------------------------------------------------------------------------------
imms :: Int -> [AnfExpr a] -> (Int, Binds a, [ImmExpr a])
--------------------------------------------------------------------------------
imms i []           = (i, [], [])
imms i (e:es)       = (i'', bs' ++ bs, e' : es' )
  where
    (i' , bs , e' ) = imm  i  e
    (i'', bs', es') = imms i' es

--------------------------------------------------------------------------------
-- | `imm i e` takes as input a "start" counter `i` and expression `e` and
--   returns an output `(i', bs, e')` where
--   * `i'` is the output counter (i.e. i' - i) anf-variables were generated,
--   * `bs` are the temporary binders needed to render `e` in ANF, and
--   * `e'` is an `imm` value (Id or Number) equivalent to `e`.
--------------------------------------------------------------------------------
imm :: Int -> AnfExpr a -> (Int, Binds a, ImmExpr a)
--------------------------------------------------------------------------------
imm i (Number n l)      = (i  , [], Number n l)

imm i (Boolean b  l)    = (i  , [], Boolean b l)

imm i (Id x l)          = (i  , [], Id x l)

imm i (Prim1 o e1 l)    = (i'', bs, mkId v l)
  where
    (i' , b1s, v1)      = imm i e1
    (i'', v)            = fresh l i'
    bs                  = (v, (Prim1 o v1 l, l)) : b1s

imm i (Prim2 o e1 e2 l) = (i'', bs', mkId x l)
  where
    (i', bs, [v1, v2])  = imms i [e1, e2]
    (i'', x)            = fresh l i'
    bs'                 = (x, (Prim2 o v1 v2 l, l)) : bs

imm i (Tuple es l)      = error "TBD:imm:Tuple"

imm i (GetItem e1 e2 l) = error "TBD:imm:Tuple"

imm i (App f es l)      = (i'', bs', mkId x l)
  where
    (i', bs, vs)        = imms  i es
    (i'', x)            = fresh l i'
    bs'                 = (x, (App f vs l, l)) : bs

imm i e@(If _ _ _  l)   = immExp i e l

imm i e@(Let _ _ _ l)   = immExp i e l


immExp :: Int -> AnfExpr a -> a -> (Int, Binds a, ImmExpr a)
immExp i e l  = (i'', bs, mkId v l)
  where
    (i' , e') = anf i e
    (i'', v)  = fresh l i'
    bs        = [(v, (e', l))]
-}


anf :: LExp ann -> Gensym (LExp (ann, Int))
anf = goT where
  goT (T ss e) = T (ss, 0) <$> goF e -- what do we need this annot for??

  goF :: ExpF Name
  goF (Lit l) = pure ([], Lit l)
  goF (Fun bnd e) = Fun bnd <$> goT e -- !!! name capture !!!
  goF (Let lets e) = undefined -- just traverse it? what about scope
  goF (App e1 e2) = liftM2 (liftA2 App) <$> goT e1 <*> goT e2
    -- do
    -- (lets1, e1') <- goT e1
    -- (lets2, e2') <- goT e2
    -- (lets1++lets2, App e1' e2')


-- mkId :: Bind a -> a -> Expr a
-- mkId x l = Id (bindId x) l

--------------------------------------------------------------------------------
-- | `fresh i` returns a temp-var named `i` and "increments" the counter
--------------------------------------------------------------------------------
freshAnf :: Gensym String
--------------------------------------------------------------------------------
freshAnf = ("a"<>)<$>gensym


{-
anf1 := "add1(add1(add1(add1(x))))"

let t1 = add1(x)
  , t2 = add1(t1)
  , t3 = add1(t2)
  , t4 = add1(t3)
in
    t4

anf2 := ((2 + 3) * (12 - 4)) * (7 + 8)

 let t1 = 2  + 3
   , t2 = 12 - 4
   , t3 = t1 * t2
   , t4 = 7 + 8
 in
     t3 * t4

anf3 := (let x = 10 in x + 5) + (let y = 20 in y - 5)

let  t1 = let x = 10 in
           x + 5
   , t2 = let y = 20 in
           y - 5
in
   t1 + t2

anf4 := (if x: y + 1 else: z + 1) + 12
let t = (if ...)
in t + 12

-}
