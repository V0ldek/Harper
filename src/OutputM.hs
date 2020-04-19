{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module OutputM where
import           Control.Monad                  ( MonadPlus(..)
                                                , liftM
                                                )
import           Control.Applicative            ( Applicative(..)
                                                , Alternative(..)
                                                )
import           Control.Monad.Reader
import           Control.Monad.State

import           ErrM

data Output o a = Out o (Err a)
  deriving (Read, Show, Eq, Ord)

class (Monoid o, Monad m) => MonadOutput o m | m -> o where
    output :: o -> m ()

instance Monoid o => MonadOutput o (Output o) where
  output o = Out o (return ())

instance Monoid o => Monad (Output o) where
  return = Out mempty . return
  fail   = Out mempty . fail
  (Out o e) >>= f = do
    let (Out o' b) = case e of
          Ok  a -> f a
          Bad s -> fail s
    Out (o `mappend` o') b

instance Monoid o => Applicative (Output o) where
  pure = return
  (Out o e) <*> (Out o' e') =
    let b = case (e, e') of
          (Bad s, _    ) -> Bad s
          (Ok  f, Ok b ) -> Ok (f b)
          (Ok  f, Bad s) -> Bad s
    in  Out (o `mappend` o') b

instance Monoid o => Functor (Output o) where
  fmap = liftM

instance Monoid o => MonadPlus (Output o) where
  mzero = Out mempty mzero
  mplus (Out o e) (Out o' e') = Out (o `mappend` o') (e `mplus` e')

instance Monoid o => Alternative (Output o) where
  empty = mzero
  (<|>) = mplus
