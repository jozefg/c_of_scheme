{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances                                                 #-}
module Gen where
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Applicative

newtype GenT m a = GenT {unGenT :: StateT Integer m a}
                    deriving(Functor, Applicative, Monad)
type Gen = GenT Identity

instance MonadTrans GenT where
  lift = GenT . lift

instance MonadReader r m => MonadReader r (GenT m) where
  local f = GenT . local f . unGenT
  ask     = GenT ask
instance MonadState s m => MonadState s (GenT m) where
  get    = GenT $ lift get
  put    = GenT . lift . put
instance (MonadWriter w m) => MonadWriter w (GenT m) where
  tell m = lift $ tell m
  listen = GenT . listen . unGenT
  pass   = GenT . pass . unGenT

class MonadGen m where
  gen :: m Integer

instance (Monad m, Functor m) => MonadGen (GenT m) where
  gen = GenT $ modify (+1) >> get

instance (MonadGen m, Monad m) => MonadGen (StateT s m)  where
  gen = lift gen

instance (MonadGen m, Monad m) => MonadGen (ReaderT s m)  where
  gen = lift gen

instance (MonadGen m, Monad m, Monoid s) => MonadGen (WriterT s m)  where
  gen = lift gen

runGenT :: Monad m => GenT m a -> m a
runGenT = flip evalStateT 0 . unGenT

runGen :: Gen a -> a
runGen = runIdentity . runGenT
