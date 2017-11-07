module Parser where

import Control.Applicative hiding (many, optional)
import Control.Monad

newtype Parser a = Parser { apply :: String -> [(a, String)] }

parse :: Parser a -> String -> a
parse p = fst . head . apply p

instance Functor Parser where
  fmap f p = Parser $ 
               \s -> [ (f a, s') | (a, s') <- apply p s ]

instance Applicative Parser where
  pure a  = Parser $ \s -> [(a, s)]
  f <*> p = Parser $ 
             \s -> [ (f' a, s'') | (f', s')  <- apply f s,
                                  (a, s'') <- apply p s' ]

instance Monad Parser where
  return = pure
  p >>= q = Parser $ 
             \s -> [ (y, s'') | (x, s')  <- apply p s,
                                (y, s'') <- apply (q x) s']

instance Alternative Parser where
  empty = Parser $ \s -> []
  p <|> q = Parser $ 
                \s -> let ps = apply p s in 
                        if null ps then apply q s else ps

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)
  
