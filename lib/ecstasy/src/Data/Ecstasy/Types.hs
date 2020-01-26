{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -funbox-strict-fields   #-}

module Data.Ecstasy.Types where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Writer.Class (MonadWriter)
import Data.Data
import Data.IORef (IORef)
import Data.IntMap.Strict (IntMap)
import Data.Kind
import GHC.Generics (Generic)
import Lens.Micro


------------------------------------------------------------------------------
-- | The key for an entity.
newtype Ent = Ent { unEnt :: Int }
  deriving (Eq, Ord, Data, Typeable)

instance Show Ent where
  show (Ent e) = "Ent " ++ show e


------------------------------------------------------------------------------
-- | The internal state of the 'SystemT' monad.
data SystemState w = SystemState
  { _ssNextId :: {-# UNPACK #-} !Int
  , _ssWorld  :: w 'WorldOf
  } deriving (Generic)

ssNextId :: Lens' (SystemState w) Int
ssNextId f (SystemState a b) = (\a' -> SystemState a' b) <$> f a

ssWorld :: Lens' (SystemState w) (w 'WorldOf)
ssWorld f (SystemState a b) = (\b' -> SystemState a b') <$> f b


------------------------------------------------------------------------------
-- | A monad transformer over an ECS given a world 'w'.
newtype SystemT w m a = SystemT
  { runSystemT' :: ReaderT (IORef (SystemState w)) m a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState s
           , MonadWriter ww
           , MonadIO
           )

instance MonadTrans (SystemT w) where
  lift = SystemT . lift

instance MonadReader r m => MonadReader r (SystemT w m) where
  ask = SystemT $ lift ask
  local z s = SystemT . ReaderT $ \r ->
    local z $ runReaderT (runSystemT' s) r


------------------------------------------------------------------------------
-- | A computation to run over a particular entity.
newtype QueryT w m a = QueryT
  { runQuery' :: ReaderT (Ent, SystemState w) (MaybeT m) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , Alternative
           , MonadPlus
           , MonadFail
           )

instance MonadTrans (QueryT w) where
  lift = QueryT . lift . lift



------------------------------------------------------------------------------
-- | Data kind used to parameterize the ECS record.
data StorageType
  = FieldOf   -- ^ Used to describe the actual entity.
  | WorldOf   -- ^ Used to construct the world's storage.
  | SetterOf  -- ^ Used to construct a setter to update an entity.


------------------------------------------------------------------------------
-- | Data kind used to parameterize the fields of the ECS record.
data ComponentType
  = Field      -- ^ This component can be owned by any entity.
  | Unique     -- ^ This component can be owned by only a single entity at a time.


------------------------------------------------------------------------------
-- | Describes how we can change an 'a'.
data Update a
  = Keep   -- ^ Keep the current value.
  | Unset  -- ^ Delete the current value if it exists.
  | Set !a  -- ^ Set the current value.
  | Modify !(a -> a)


------------------------------------------------------------------------------
-- | A type family to be used in your ECS recrod.
type family Component (s :: StorageType)
                      (c :: ComponentType)
                      (a :: Type) :: Type where
  Component 'FieldOf  c       a = Maybe a
  Component 'SetterOf c       a = Update a

  Component 'WorldOf 'Field   a = IntMap a
  Component 'WorldOf 'Unique  a = Maybe (Int, a)


------------------------------------------------------------------------------
-- | The inverse of 'Component 'WorldOf' -- used to prove 'IsInjective'
type family Inverse (r :: Type) :: (ComponentType, Type) where
  Inverse (IntMap a)       = '( 'Field, a)
  Inverse (Maybe (Int, a)) = '( 'Unique, a)

------------------------------------------------------------------------------
-- | A proof that 'c' is injective.
class ('(c, a) ~ Inverse (Component 'WorldOf c a))
    => IsInjective (c :: ComponentType) a
instance ('(c, a) ~ Inverse (Component 'WorldOf c a))
    => IsInjective (c :: ComponentType) a

