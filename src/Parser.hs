{-# LANGUAGE InstanceSigs #-}

module Parser where

import Data.Char (isSpace)
import Data.Tuple (swap)
import Control.Applicative


newtype StateT s m a = StateT
  { runStateT :: s -> m (a, s) }

instance Monad m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT r) = StateT $ \state -> do
    (value, state') <- r state
    return (f value, state')

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \state -> pure (x, state)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT f <*> StateT r = StateT $ \state -> do
    (g, state') <- f state
    (x, state'') <- r state'
    return (g x, state'')

instance (Alternative m, Monad m) => Alternative (StateT s m) where
  empty :: StateT s m a
  empty = StateT $ \_ -> empty

  (<|>) :: StateT s m a -> StateT s m a -> StateT s m a
  StateT r <|> StateT s = StateT $ \state ->
    r state <|> s state

instance Monad m => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT r >>= f = StateT $ \state -> do
    (value, state') <- r state
    (value', state'') <- run (f value) state'
    return (value', state'')
    where run :: StateT s m b -> s -> m (b, s)
          run (StateT p) s = p s

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (pure . f)

get :: Monad m => StateT s m s
get = state $ \s -> (s, s)

put :: Monad m => s -> StateT s m ()
put s = state $ \_ -> ((), s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = get >>= (put . f)

evalStateT :: Monad m => s -> StateT s m a -> m a
evalStateT s m = fst <$> runStateT m s


data State = State Int Int
  deriving (Eq, Show)

instance Semigroup State where
  State x y <> State x' y' = State (x + x') (y + y')
  
instance Monoid State where
  mempty = State 0 0


addRow :: Int -> State -> State
addRow n (State row col) = State (row + n) 1

addCol :: Int -> State -> State
addCol n (State row col) = State row (col + n)

initialState :: State
initialState = State 1 1


data ParserError = ParserError (State, String)

instance Show ParserError where
  show (ParserError (State row col, err)) = concat
    [ "Parse Error ("
    , show row
    , ":"
    , show col
    , "): "
    , err
    ]

instance Semigroup ParserError where
  x <> y = x

instance Monoid ParserError where
  mempty = ParserError (mempty, mempty)


newtype Parser a = Parser
  { runParser :: String -> StateT State (Either ParserError) (String, a) }


instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input -> do
    (input', x) <- p input
    return (input', f x)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> pure (input, x)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser f <*> Parser p = Parser $ \input -> do
    (input', g) <- f input
    (input'', x) <- p input'
    return (input'', g x)

instance Monoid e => Alternative (Either e) where
  empty :: Either e a
  empty = Left mempty

  (<|>) :: Either e a -> Either e a -> Either e a
  a <|> b = a <> b

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> empty

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser p <|> Parser q = Parser $ \input ->
    p input <|> q input

flatParser :: Parser (Parser a) -> Parser a
flatParser (Parser p) = Parser $ \input -> do
  (input', Parser q) <- p input
  (input'', x) <- q input'
  return (input'', x)

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  x >>= f = flatParser (fmap f x)


parseError :: String -> StateT State (Either ParserError) (String, a)
parseError err = StateT $ \state -> (Left . ParserError) (state, err)

expected :: String -> String -> String
expected a b = "Unexpected '" ++ b ++ "'. Expecting '" ++ a ++ "'"


char :: Char -> Parser Char
char c = Parser p
  where p :: String -> StateT State (Either ParserError) (String, Char)
        p (s:str)
          | (s == c) && (c == '\n') = do
              modify $ addRow 1
              return (str, s)
          | s == c = do
              modify $ addCol 1
              return (str, s)
          | s /= c = parseError (expected [c] [s])
        p _ = parseError (expected [c] "end of file")

string :: String -> Parser String
string = traverse char

spanP :: (Char -> Bool) -> Parser String
spanP pred = many $ Parser p
  where p (s:str)
          | pred s = do
              if s == '\n' then modify (addRow 1) else modify (addCol 1)
              return (str, s)
          | otherwise = parseError "Unmatched predicate"
        p _ = parseError "Unmatched predicate"

-- Not used
notNull :: String -> Parser [a] -> Parser [a]
notNull errMsg (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs
    then parseError errMsg
    else return (input', xs)

noneOf :: [Char] -> Parser Char
noneOf blacklist = Parser h
  where h (s:str)
          | s `notElem` blacklist = do
              if s == '\n'
                then modify $ addRow 1
                else modify $ addCol 1
              return (str, s)
          | s `elem` blacklist = parseError ("Unexpected " ++ [s])
        h _ = parseError "Unexpected end of file"

anyOf :: [Char] -> Parser Char
anyOf whitelist = Parser h
  where h (s:str)
          | s `elem` whitelist = do
              if s == '\n'
                then modify $ addRow 1
                else modify $ addCol 1
              return (str, s)
          | s `notElem` whitelist = parseError ("Unexpected " ++ [s])
        h _ = parseError "Unexpected end of file"

anyChar :: Parser Char
anyChar = Parser $ h
  where h (s:str) = do
          if s == '\n'
            then modify $ addRow 1
            else modify $ addCol 1
          return (str, s)
        h _ = parseError "Unexpected end of file"

space :: Parser String
space = spanP isSpace

token :: Char -> Parser Char
token t = space *> char t <* space

comma :: Parser Char
comma = token ','

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy element sep = liftA2 (:) element (many (sep *> element))
  <|> pure []

endBy :: Parser a -> Parser b -> Parser a
endBy element end = element <* end

eol :: Parser String
eol =
  string "\n\r"
  <|> string "\r\n"
  <|> string "\n"
  <|> string "\r"

eof :: Parser ()
eof = Parser h
  where h "" = pure ("", ())
        h _  = parseError "Expecting end of file"
