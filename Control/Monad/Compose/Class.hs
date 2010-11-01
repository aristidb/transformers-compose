{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}

{- |
Copyright    : 2010 Aristid Breitkreuz
License      : BSD3
Stability    : experimental
Portability  : portable

This module provides Arrow-like monad composition for transformers. To be more precise, it is "Category-like",
i.e. the parallels are to 'Control.Category.Category'.

/This version has been adapted from monadLib-compose, to work with the transformers package./

'Control.Category.Category' generalises '.' and 'id' to arrows and categories. One such arrow is 'Kleisli',
which represents functions returning monadic values. Incidentally, that's equivalent to 'ReaderT'! So it
turns out that it is possible to generalise '.' and 'id' to 'ReaderT' ('id' is just 'ask'), as well as to
many monad transformer stacks that embed a 'ReaderT' inside.
-}


module Control.Monad.Compose.Class
(
  MonadCompose(..)
, (<<<)
, (>>>)
)
where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Error
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Monoid
import qualified Control.Monad.Trans.RWS.Lazy      as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict    as StrictRWS
import qualified Control.Monad.Trans.State.Lazy    as Lazy
import qualified Control.Monad.Trans.State.Strict  as Strict
import qualified Control.Monad.Trans.Writer.Lazy   as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
--import           Control.Monad.Logic               (LogicT(..), runLogicT)

-- | Composable monads. Compare with 'Control.Category.Category'.
-- Note that there are two different monad types involved in each instance.
class (Monad m, Monad n) => MonadCompose m n s t | m -> s, n -> t, n s -> m where
    -- | Compose two monadic values from right to left. @mcompose f g@ is 
    -- comparable to @f . g@ but for monadic values. Compare with 'Control.Category..'.
    mcompose :: m a -> n s -> n a
    mcompose m n = mapply m =<< n
    
    -- | Apply a constant value to a composable monad.
    mapply :: m a -> s -> n a
    mapply m s = mcompose m (return s)

-- | Compose two monadic values from right to left. Compare with 'Control.Category.<<<'.
-- @f <<< g@ is equivalent to @mcompose f g@.
(<<<) :: MonadCompose m n s t => m a -> n s -> n a
(<<<) = mcompose
infixr 1 <<<

-- | Compose two monadic values from left to right. Compare with 'Control.Category.>>>'.
-- @g >>> f@ is equivalent to @mcompose f g@.
(>>>) :: MonadCompose m n s t => n s -> m a -> n a
(>>>) = flip mcompose
infixl 1 >>>

instance MonadCompose ((->) s) ((->) t) s t where
    mcompose = (.)

instance Monad m => MonadCompose (ReaderT s m) (ReaderT t m) s t where
    mapply m a = ReaderT $ \_ -> runReaderT m a

x_mapply :: (MonadTrans xt, MonadCompose m n s t, Monad (xt n)) 
            => (a -> xt n b) -> (xt m c -> m a) -> xt m c -> s -> xt n b
x_mapply close open m s = lift (open m `mapply` s) >>= close

x_mapply' :: (MonadTrans xt, MonadCompose m n s t, Monad (xt n)) 
            => (n a -> xt n b) -> (xt m c -> m a) -> xt m c -> s -> xt n b
x_mapply' close' open = x_mapply (close' . return) open

instance MonadCompose m n s t => MonadCompose (IdentityT m) (IdentityT n) s t where
    mapply = x_mapply return runIdentityT

instance MonadCompose m n s t => MonadCompose (MaybeT m) (MaybeT n) s t where
    mapply = x_mapply' MaybeT runMaybeT

instance (MonadCompose m n s t, Error e) => MonadCompose (ErrorT e m) (ErrorT e n) s t where
    mapply = x_mapply' ErrorT runErrorT

instance MonadCompose m n s t => MonadCompose (Lazy.StateT i m) (Lazy.StateT i n) s t where
    mapply m a = Lazy.StateT $ \i -> mapply (Lazy.runStateT m i) a

instance MonadCompose m n s t => MonadCompose (Strict.StateT i m) (Strict.StateT i n) s t where
    mapply m a = Strict.StateT $ \i -> mapply (Strict.runStateT m i) a

instance (MonadCompose m n s t, Monoid w) => MonadCompose (Lazy.WriterT w m) (Lazy.WriterT w n) s t where
    mapply = x_mapply' Lazy.WriterT Lazy.runWriterT

instance (MonadCompose m n s t, Monoid w) => MonadCompose (Strict.WriterT w m) (Strict.WriterT w n) s t where
    mapply = x_mapply' Strict.WriterT Strict.runWriterT

instance (Monad m, Monoid w) => MonadCompose (LazyRWS.RWST s w i m) (LazyRWS.RWST t w i m) s t where
    mapply m a = LazyRWS.RWST $ \_ i -> LazyRWS.runRWST m a i

instance (Monad m, Monoid w) => MonadCompose (StrictRWS.RWST s w i m) (StrictRWS.RWST t w i m) s t where
    mapply m a = StrictRWS.RWST $ \_ i -> StrictRWS.runRWST m a i

{-
instance MonadCompose m n s t => MonadCompose (LogicT m) (LogicT n) s t where
    mcompose m n = LogicT $ \sk fk -> runLogicT (mcompose m n) sk fk
-}
