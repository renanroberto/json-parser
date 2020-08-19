{-# LANGUAGE InstanceSigs #-}

module State where

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
