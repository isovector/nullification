{-# LANGUAGE TypeSynonymInstances #-}

module Prelude
  ( module Prelude
  , module BasePrelude
  , module Game.Sequoia
  , module Control.Lens
  , module Data.Ecstasy
  , module Linear
  , module Game.Sequoia.Window
  , module Control.Monad.Trans.Class
  , module Types
  , request
  ) where

import           BasePrelude hiding (group, rotate, lazy, index, uncons, loop, inRange, yield, Down (..))
import           Control.Lens hiding (without, op, over)
import           Control.Monad.Coroutine.SuspensionFunctors (request)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer.CPS
import qualified Data.Ecstasy as E
import           Data.Ecstasy hiding (query, queryEnt, queryMaybe, queryDef, allEnts, newEntity, queryUnique, queryTarget, subquery)
import           Data.Ecstasy.Internal.Deriving
import           Data.Ecstasy.Types
import qualified Data.Ecstasy.Types as E
import           Game.Sequoia hiding (form, change, play)
import           Game.Sequoia.Window (MouseButton (..))
import           Linear (norm, normalize, (*^), (^*), quadrance, M22, project)
import           Types


class CanRunCommands m where
  commands :: [Command] -> m ()

instance CanRunCommands Query where
  commands = lift . tell

instance CanRunCommands Game where
  commands = lift . tell

instance CanRunCommands Task where
  commands = lift . commands

command :: CanRunCommands m => Command -> m ()
command = commands . pure


class Monad m => CanRunQueries m where
  query
      :: ( GetField c, Inverse UnderlyingMonad (Component ('WorldOf UnderlyingMonad) c a) ~ c)
      => (EntWorld ('WorldOf UnderlyingMonad) -> Component ('WorldOf UnderlyingMonad) c a)
      -> m a
  queryUnique
      :: (EntWorld ('WorldOf UnderlyingMonad) -> Component ('WorldOf UnderlyingMonad) 'Unique a)
      -> m (Maybe (Ent, a))
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
  queryTarget
      :: EntTarget EntWorld UnderlyingMonad -> m [Ent]
  subquery
      :: EntTarget EntWorld UnderlyingMonad
      -> Query a
      -> m [a]
  focus :: Ent -> Query a -> m a

instance CanRunQueries Query where
  query       = E.query
  subquery    = E.subquery
  queryUnique = E.queryUnique
  queryMaybe  = E.queryMaybe
  queryDef    = E.queryDef
  queryEnt    = E.queryEnt
  queryTarget = E.queryTarget
  focus e m   = E.QueryT $ local (first $ const e) $ runQueryT' m

instance CanRunQueries Task where
  query       = lift . E.query
  subquery    = (lift .) . E.subquery
  queryUnique = lift . E.queryUnique
  queryMaybe  = lift . E.queryMaybe
  queryDef    = (lift .) . E.queryDef
  queryEnt    = lift E.queryEnt
  queryTarget = lift . E.queryTarget
  focus e m   = lift . E.QueryT $ local (first $ const e) $ runQueryT' m


allEnts :: MonadIO m => EntTarget EntWorld m
allEnts = entsWith eAlive

newEntity :: EntWorld 'FieldOf
newEntity = E.newEntity { eAlive = Just () }


yield :: EntWorld 'SetterOf -> Task ()
yield = void . request

