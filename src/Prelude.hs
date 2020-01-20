{-# LANGUAGE TypeSynonymInstances #-}

module Prelude
  ( module Prelude
  , module BasePrelude
  , module Game.Sequoia
  , module Game.Sequoia.Utils
  , module Control.Lens
  , module Data.Ecstasy
  , module Linear
  , module Game.Sequoia.Window
  , module Control.Monad.Trans.Class
  , module Types
  , request
  ) where

import Control.Monad.Trans.Reader
import BasePrelude hiding (group, rotate, lazy, index, uncons, loop, inRange, yield)
import Control.Lens hiding (without, op, over)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Data.Ecstasy hiding (query, queryEnt, queryMaybe, queryDef)
import qualified Data.Ecstasy as E
import qualified Data.Ecstasy.Types as E
import Data.Ecstasy.Internal.Deriving
import Data.Ecstasy.Types
import Game.Sequoia hiding (form, change)
import Game.Sequoia.Utils (showTrace)
import Game.Sequoia.Window (MouseButton (..))
import Linear (norm, normalize, (*^), (^*), quadrance, M22, project)
import Control.Monad.Trans.Writer.CPS
import Types
import Control.Monad.Coroutine.SuspensionFunctors (request)


class CanRunCommands m where
  command :: Command -> m ()

instance CanRunCommands Query where
  command = lift . tell . pure

instance CanRunCommands Game where
  command = lift . tell . pure

instance CanRunCommands Task where
  command = lift . lift . tell . pure


class Monad m => CanRunQueries m where
  query
      :: (GetField c, Inverse UnderlyingMonad (Component ('WorldOf UnderlyingMonad) c a) ~ c)
      => (EntWorld ('WorldOf UnderlyingMonad) -> Component ('WorldOf UnderlyingMonad) c a)
      -> m a
  queryMaybe
      :: (GetField c, Inverse UnderlyingMonad (Component ('WorldOf UnderlyingMonad) c a) ~ c)
      => (EntWorld ('WorldOf UnderlyingMonad) -> Component ('WorldOf UnderlyingMonad) c a)
      -> m (Maybe a)
  queryDef
      :: (GetField c, Inverse UnderlyingMonad (Component ('WorldOf UnderlyingMonad) c a) ~ c)
      => a
      -> (EntWorld ('WorldOf UnderlyingMonad) -> Component ('WorldOf UnderlyingMonad) c a)
      -> m a
  queryEnt
      :: m Ent
  focus :: Ent -> Query a -> m a

instance CanRunQueries Query where
  query      = E.query
  queryMaybe = E.queryMaybe
  queryDef   = E.queryDef
  queryEnt   = E.queryEnt
  focus e m  = E.QueryT $ local (first $ const e) $ runQueryT' m

instance CanRunQueries Task where
  query      = lift . E.query
  queryMaybe = lift . E.queryMaybe
  queryDef   = (lift .) . E.queryDef
  queryEnt   = lift E.queryEnt
  focus e m  = lift . E.QueryT $ local (first $ const e) $ runQueryT' m


yield :: EntWorld 'SetterOf -> Task ()
yield = void . request

