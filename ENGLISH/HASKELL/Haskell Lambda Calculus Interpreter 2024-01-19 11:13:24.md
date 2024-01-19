```haskell
import Control.Monad (liftM, ap, foldM)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map

data Term = Var String | App Term Term | Lam String Term deriving Show

instance Eq Term where
  (==) = (==) `on` show

instance Ord Term where
  compare = comparing show

instance Semigroup Term where
  (<>) = App

-- | map M a -> M (B a)
fmapM :: (Functor M, Monoid B) => (a -> B a) -> M a -> M (B a)
fmapM f = ap (pure f)

-- | Map m a -> B a -> B m a
mappendM :: (Monad m, Monoid B) => Map a m -> B a -> m (B m a)
mappendM m x = fmapM (<>) m `ap` pure x

-- | (Functor m, Monoid B1, Monoid B2) =>
--   B1 -> B2 -> (B1 -> B2) -> m B1 -> m B2
ext :: Functor m
       => Monoid b1
       => Monoid b2
       => b1 -> b2 -> (b1 -> b2) -> m b1 -> m b2
ext e f g = fmapM (\a -> g a + f)

-- | convert a term into a string
showTerm :: Term -> String
showTerm (Var x) = x
showTerm (App t u) = "(" ++ showTerm t ++ " " ++ showTerm u ++ ")"
showTerm (Lam x t) = "\\" ++ x ++ ". " ++ showTerm t

-- | convert a string into a term
readTerm :: String -> Term
readTerm s = case pExpr s of
  [x] -> x
  _ -> error "failed to parse term"
  where pExpr = do {
    skipSpace;
    t <- pTerm;
    skipSpace;
    eof;
    return t
  }

  pTerm = do {
    skipSpace;
    x <- pVar;
    skipSpace;
    t <- pApp;
    return $ x & t
  } <|> pAbs <|> pLam <|> pVar

  pApp  = App <$> pAtom <*> pTerm
  pAtom = do {
    skipSpace;
    x <- pVar;
    skipSpace;
    return x
  } <|> pParens

  pParens = do {
    skipSpace;
    pStr "(";
    t <- pTerm;
    skipSpace;
    pStr ")";
    return t
  }

  pAbs = do {
    skipSpace;
    pStr "\\";
    skipSpace;
    x <- pVar;
    skipSpace;
    pStr ".";
    skipSpace;
    t <- pTerm;
    return $ Lam x t
  }

  pLam  = do {
    skipSpace;
    pStr "lambda";
    skipSpace;
    x <- pVar;
    skipSpace;
    pStr ".";
    skipSpace;
    t <- pTerm;
    return $ Lam x t
  }

  pVar = do {
    skipSpace;
    x <- pIdent;
    skipSpace;
    return $ Var x
  }

  pIdent = do {
    skipSpace;
    xs <- pLetter <|> pDigit;
    skipSpace;
    x <- many $ pLetter <|> pDigit;
    return $ xs & x
  }

  pLetter = elemSatisfies (isLetter . fromEnum) BS.singleton
  pDigit  = elemSatisfies (isDigit . fromEnum) BS.singleton

  skipSpace = many $ char `in` " \t\n"
  eof      = void $ char `notElem` " \t\n"
  pStr     s = void $ string s

  char :: Eq a => a -> Parser a
  char c = do {
    x <- peekChar;
    unless (x == c) $ fail "expected a character"
    void $ getChar;
    return c
  }

  string :: String -> Parser String
  string = foldM go ""
  where
    go acc c = do {
      x <- expect c;
      return $ acc ++ x;
    }

  many :: Parser a -> Parser [a]
  many p = many1 p <|> pure []

  many1 :: Parser a -> Parser [a]
  many1 p = do {
    x <- p;
    xs <- many p;
    return $ x:xs
  }

  elemSatisfies :: (a -> Bool) -> ByteString -> Parser a
  elemSatisfies p = do {
    x <- getChar;
    unless (p x) $ fail "expected a satisfying element"
    return x
  }

  void :: Monad m => m () -> m a
  void m = m >> return ()

  expect :: Eq a => a -> Parser a
  expect x = do {
    y <- getChar;
    unless (y == x) $ fail "expected a character"
    return y
  }

  peekChar :: Monad m => m Char
  peekChar = getChar >>= putChar >> getChar

-- | evaluate a term given a variable assignment
eval :: Map String ByteString -> Term -> ByteString
eval env (Var x) = fromMaybe (error $ "unbound variable: " ++ x) (Map.lookup x env)
eval env (App t u) = eval env t `BS.append` eval env u
eval env (Lam x t) = pack (x & t)

-- | read a term from a string
readTermFromString :: String -> Either String Term
readTermFromString = liftM readTerm . BS.unpack . BS.trim

-- | evaluate a term from a string given a variable assignment
evalTermFromString :: Map String ByteString -> String -> Either String ByteString
evalTermFromString env s = (eval env <$> readTermFromString s)

-- | print a term to a string
showTermToString :: Term -> String
showTermToString = BS.unpack . BS.pack . showTerm

-- | main function
main :: IO ()
main = do {
  let env = Map.fromList [("x", "hello"), ("y", "world")];
  let t = readTerm "lambda x. \\y. x ++ y";
  let r = eval env t;
  putStrLn $ showTermToString t;
  putStrLn $ BS.unpack r
}
```

This code is an implementation of a lambda calculus interpreter in Haskell.

The `Term` data type represents lambda calculus terms, which can be variables, applications, or abstractions.

The `showTerm` function converts a lambda calculus term into a string.

The `readTerm` function converts a string into a lambda calculus term.

The `eval` function evaluates a lambda calculus term given a variable assignment.

The `evalTermFromString` function evaluates a lambda calculus term from a string given a variable assignment.

The `showTermToString` function converts a lambda calculus term into a string.

The `main` function is the main entry point of the program. It creates a variable assignment, reads a lambda calculus term from a string, evaluates the term, and prints the result.