{-# LANGUAGE TypeApplications, ScopedTypeVariables, ExistentialQuantification #-}

module Language.Lamb.Parser
  -- ( parse
  -- , parseFile
  -- )
where

import           Control.Monad (void)
import           Control.Monad.State.Strict
import           Data.Either
import           Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import           Language.Lamb.AST
import           Language.Lamb.UX --Types(Located, PPrint, SourceSpan(..))
import           Text.Megaparsec hiding (State, parse)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr
import Debug.Trace
import Data.Word
import Data.Int
type Parser = ParsecT SourcePos Text (State PCtx)

newtype PCtx = PCtx
  { scopedIdents :: S.Set String
  }

emptyPCtx = PCtx mempty

traceMe _ | True = pure ()
traceMe x = traceM x

traceParser _ p | True = p
traceParser s p = do
 p' <- p
 traceM $ "PARSED: ("<>s<>") " --  <>show p'
 pure p'

--------------------------------------------------------------------------------
parseFile :: FilePath -> IO (LMod SourceSpan)
--------------------------------------------------------------------------------
parseFile f = parse f <$> readFile f

--------------------------------------------------------------------------------
parse :: FilePath -> Text -> LMod SourceSpan
----------------------------------------------------------------------------------
parse = parseWith parseMod

parseWith  :: Parser a -> FilePath -> Text -> a
parseWith p f s = either fail id $ evalState (runParserT (whole p) f s) emptyPCtx
  where
   fail err = panic (show err) (posSpan . NE.head . errorPos $ err)

-- https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html
instance Located (ParseError a b) where
  sourceSpan = posSpan . NE.head . errorPos

instance (Show a, Show b) => PPrint (ParseError a b) where
  pprint = show

--------------------------------------------------------------------------------
-- | Large units
----------------------------------------------------------------------------------

parseMod :: Parser (LMod SourceSpan)
parseMod = wrap $ Mod <$> name <*> decls
  where
    name = traceParser "parseModName" $ rword "module" *> pModuleName <* rword "where" <* sc
    decls = traceParser "parseModDecls" $  many (parseDecl<* semi)

parseDecl :: Parser (LDecl SourceSpan)
parseDecl = wrap $ scDecl -- <|> dtDecl
  where
    scDecl = traceParser "parseScDecl" $ uncurry Sc <$> letBinder1 parseELit

-- TODO figure out AST for types.
{-
    dtDecl = do
      rword "type"
      name <- fst <$> uident
      params <- many ident
      equal
      cases <- sepBy1 product (char '|')
      -- sumTree attempts to make a balanced tree of cases
      pure $ Dt name (types params (sumTree cases))
-}

binops :: [[Operator Parser (T SourceSpan (ExpF String lit))]]
binops =
  [ [ InfixL (op2 "*"  )
    ]
  , [ InfixL (op2 "+"  )
    , InfixL (op2 "-"  )
    ]
  , [ InfixL (op2 "==" )
    , InfixL (op2 ">"  )
    , InfixL (op2 "<"  )
    ]
  ]
  where
    op2 s = do
      (op, ss) <- withSpan (symbol s)
      pure $ \e1 e2 -> apps (<>) (T ss (Bnd op)) [e1, e2]
--    op2 o e1 e2 = Prim2 o e1 e2 (stretch [e1, e2])


-- parse the core language.
-- if this gets more complicated, then add a functor for the surface language
-- and a desugaring step that lifts a natural transformation of surface ~> core
-- over the cofree comonad.

parseExpr pl = makeExprParser (parseExpr0 pl) binops

parseExpr0 pl = parseApp pl

parseExpr1 :: Parser lit -> Parser (T SourceSpan (ExpF String lit))
parseExpr1 pl =
  choice [
    try $ parseFun pl
  , try $ parseLet pl
--  , try $ parseApp pl
  , try $ parseTerminal pl
  , parens (parseExpr pl)
  ]

parseTerminal pl = choice[
  try $ parseLit pl
  , try $ parseBind
  ]


-- expr0 :: Parser Bare
-- expr0 =  try primExpr
--      <|> try letExpr
--      <|> try ifExpr
--      <|> try getExpr
--      <|> try appExpr
--      <|> try tupExpr
--      <|> try constExpr
--      <|> idExpr

-- exprs :: Parser [Bare]
-- exprs = parens (sepBy1 expr comma)

--------------------------------------------------------------------------------
-- | Parsers for ExpF
--------------------------------------------------------------------------------
parseBind :: Parser (T SourceSpan (ExpF String lit))
parseBind = traceParser "parseBind" $ do
  (i, ss) <- ident
  pure $ T ss (Bnd i)

parseFun :: Parser lit -> Parser (T SourceSpan (ExpF String lit))
parseFun pl = traceParser "parseFun" $ do
  rword "fun"
  traceMe "HERE"
  args <- some ident
  tok "=>"
  traceMe "HERE2"
  body <- parseExpr pl
  traceMe "HERE3"
  pure $ funs args body

parseApp :: Parser lit -> Parser (T SourceSpan (ExpF String lit))
parseApp pl =
  traceParser "parseApp" $ go (parseExpr1 pl)
  where
    go p1 = do
      e <- p1
      go (wrap (App e <$> p1)) <|> pure e

parseLet :: Parser lit -> Parser (T SourceSpan (ExpF String lit))
parseLet pl = wrap $ traceParser "parseLet" $ do
  binds <- sepBy1 (letBinder1 pl) semi
  semi
  e <- parseExpr pl
  pure $ Let binds e

letBinder1 pl = do
  rword "let"
  name <- fst <$> ident
  args <- many ident
  equal
  exp <- parseExpr pl
  pure $ (name, funs args exp)

parseLit :: Parser lit -> Parser (T SourceSpan (ExpF String lit))
parseLit = traceParser "parseLit" . wrap . fmap Lit

--------------------------------------------------------------------------------
-- | ELit
--------------------------------------------------------------------------------

parseELit :: Parser ELit
parseELit = choice [ cbool, cword8 ]
  where
    cbool = fmap CBool (
      (rword "True" *> pure True) <|>
      (rword "False" *> pure False)
      )

    cword8 = CUInt8 . fromIntegral . fst <$> integer

-- inRange :: forall a. (Show a, Bounded b) => Parser a -> Parser a
-- inRange p = p >>= \v ->
--   if v >= minBound @a && v <= maxBound @a
--   then pure v
--   -- todo: better error
--   else fail $ show v <> " not in range"

--------------------------------------------------------------------------------
-- | Names
--------------------------------------------------------------------------------

pModuleName :: Parser String
pModuleName = intercalate "." . map fst <$> sepBy1 uident (char '.')

--------------------------------------------------------------------------------
-- | Tokenisers and Whitespace
--------------------------------------------------------------------------------

-- | Top-level parsers (should consume all input)
whole :: Parser a -> Parser a
whole p = sc *> p <* eof

-- RJ: rename me "space consumer"
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

-- | `symbol s` parses just the string s (and trailing whitespace)
symbol :: String -> Parser String
symbol = L.symbol sc

comma :: Parser String
comma = symbol ","

colon :: Parser String
colon = symbol ":"
semi :: Parser String
semi = symbol ";"

equal :: Parser String
equal = symbol "="

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = betweenS "(" ")"

-- | 'brackets' parses something between parenthesis.
brackets :: Parser a -> Parser a
brackets = betweenS "[" "]"

betweenS :: String -> String -> Parser a -> Parser a
betweenS l r = between (symbol l) (symbol r)

-- | `lexeme p` consume whitespace after running p
lexeme :: Parser a -> Parser (a, SourceSpan)
lexeme p = L.lexeme sc (withSpan p)

tok = lexeme . string

-- | 'integer' parses an integer.
integer :: Parser (Integer, SourceSpan)
integer = lexeme L.decimal

-- | `rword`
rword   :: String -> Parser SourceSpan
rword w = snd <$> (withSpan (string w) <* notFollowedBy alphaNumChar <* sc)


-- | list of reserved words
keywords :: [Text]
keywords =
  [ "if"      , "else"
  , "true"    , "false"
  , "let"     , "in"
  , "add1"    , "sub1"  , "isNum"   , "isBool", "isTuple",  "print"
  , "def"     , "lambda"
  ]

withSpan' :: Parser (SourceSpan -> a) -> Parser a
withSpan' p = do
  p1 <- getPosition
  f  <- p
  p2 <- getPosition
  return (f (SS p1 p2))

withSpan :: Parser a -> Parser (a, SourceSpan)
withSpan p = do
  p1 <- getPosition
  x  <- p
  p2 <- getPosition
  return (x, SS p1 p2)


-- | `ident` parses idents: lower-case alphabets followed by alphas or digits
ident :: Parser (String, SourceSpan)
ident = lexeme (p >>= checkNotKeyword)
  where
    p = (:) <$> lowerChar <*> many (alphaNumChar <|> oneOf @[] "_")

uident :: Parser (String, SourceSpan)
uident = lexeme (p >>= checkNotKeyword)
  where
    p = (:) <$> upperChar <*> many (alphaNumChar <|> oneOf @[] "_")

checkNotKeyword x
  | x `elem` keywords = fail $ "keyword " ++ show x ++ " cannot be an ident"
  | otherwise = return x

-- Add a SourceSpan Annotation.
wrap :: Parser (f (T SourceSpan f)) -> Parser (T SourceSpan f)
wrap = fmap (uncurry (flip T)) . withSpan
