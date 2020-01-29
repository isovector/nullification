{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

module Data.Ecstasy.Internal where

import           Control.Applicative (empty)
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Maybe (runMaybeT)
import qualified Control.Monad.Trans.Reader as R
import           Data.Coerce
import           Data.Ecstasy.Internal.Deriving
import qualified Data.Ecstasy.Types as T
import           Data.Ecstasy.Types hiding (unEnt)
import           Data.Foldable (for_)
import           Data.IORef
import qualified Data.IntMap as IM
import           Data.KEndo
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Traversable (for)
import           GHC.Generics
import           Lens.Micro ((.~), (%~), (&))


------------------------------------------------------------------------------
-- | This class provides all of the functionality necessary to manipulate the
-- ECS.
class (Monad m, MonadIO m, HasWorld' world) => HasWorld world m where

  ----------------------------------------------------------------------------
  -- | Fetches an entity from the world given its 'Ent'.
  getEntity
      :: Ent
      -> SystemT world m (world 'FieldOf)
  default getEntity
      :: ( GGetEntity (Rep (world 'WorldOf))
                      (Rep (world 'FieldOf))
         , Generic (world 'FieldOf)
         , Generic (world 'WorldOf)
         )
      => Ent
      -> SystemT world m (world 'FieldOf)
  getEntity e = do
    w <- getWorld
    pure . to
         . gGetEntity (from w)
         $ T.unEnt e
  {-# INLINEABLE getEntity #-}



class HasWorld' world where
  ----------------------------------------------------------------------------
  -- | Transforms an entity into a setter to transform the default entity into
  -- the given one. Used by 'createEntity'.
  convertSetter
      :: world 'FieldOf
      -> world 'SetterOf
  default convertSetter
      :: ( GConvertSetter (Rep (world 'FieldOf))
                          (Rep (world 'SetterOf))
         , Generic (world 'FieldOf)
         , Generic (world 'SetterOf)
         )
      => world 'FieldOf
      -> world 'SetterOf
  convertSetter = to . gConvertSetter . from
  {-# INLINEABLE convertSetter #-}

  ----------------------------------------------------------------------------
  -- | The default world, which contains only empty containers.
  defStorage :: world 'WorldOf
  default defStorage
      :: ( Generic (world 'WorldOf)
         , GDefault 'True (Rep (world 'WorldOf))
         )
      => world 'WorldOf
  defStorage = def @'True
  {-# INLINEABLE defStorage #-}

  ----------------------------------------------------------------------------
  -- | The default entity, owning no components.
  newEntity :: world 'FieldOf
  default newEntity
      :: ( Generic (world 'FieldOf)
         , GDefault 'True (Rep (world 'FieldOf))
         )
      => world 'FieldOf
  newEntity = def @'True
  {-# INLINEABLE newEntity #-}

  ----------------------------------------------------------------------------
  -- | The default setter, which keeps all components with their previous value.
  unchanged :: world 'SetterOf
  default unchanged
      :: ( Generic (world 'SetterOf)
         , GDefault 'True (Rep (world 'SetterOf))
         )
      => world 'SetterOf
  unchanged = def @'True
  {-# INLINEABLE unchanged #-}

  ----------------------------------------------------------------------------
  -- | A setter which will delete the entity if its 'QueryT' matches.
  delEntity :: world 'SetterOf
  default delEntity
      :: ( Generic (world 'SetterOf)
         , GDefault 'False (Rep (world 'SetterOf))
         )
      => world 'SetterOf
  delEntity = def @'False
  {-# INLINEABLE delEntity #-}

  updateWorld
    :: Ent
    -> world 'SetterOf
    -> world 'WorldOf
    -> world 'WorldOf
  default updateWorld
    :: ( GSetEntity (Rep (world 'SetterOf)) (Rep (world 'WorldOf))
       , Generic (world 'SetterOf)
       , Generic (world 'WorldOf)
       )
    => Ent
    -> world 'SetterOf
    -> world 'WorldOf
    -> world 'WorldOf
  updateWorld e s w =
    let !x = to . gSetEntity (from s) (T.unEnt e) $ from w
     in x



instance ( Generic (world 'SetterOf)
         , Generic (world 'FieldOf)
         , Generic (world 'WorldOf)
         , GConvertSetter (Rep (world 'FieldOf))
                          (Rep (world 'SetterOf))
         , GDefault 'True  (Rep (world 'FieldOf))
         , GDefault 'False (Rep (world 'SetterOf))
         , GDefault 'True  (Rep (world 'SetterOf))
         , GDefault 'True  (Rep (world 'WorldOf))
         , GSetEntity (Rep (world 'SetterOf))
                      (Rep (world 'WorldOf))
         ) => HasWorld' world


instance ( HasWorld' world
         , Generic (world 'SetterOf)
         , Generic (world 'WorldOf)
         , Generic (world 'FieldOf)
         , GConvertSetter (Rep (world 'FieldOf))
                          (Rep (world 'SetterOf))
         , GDefault 'True  (Rep (world 'FieldOf))
         , GDefault 'False (Rep (world 'SetterOf))
         , GDefault 'True  (Rep (world 'SetterOf))
         , GDefault 'True  (Rep (world 'WorldOf))
         , GGetEntity (Rep (world 'WorldOf))
                      (Rep (world 'FieldOf))
         , GSetEntity (Rep (world 'SetterOf))
                      (Rep (world 'WorldOf))
         , Monad m
         , MonadIO m
         ) => HasWorld world m


------------------------------------------------------------------------------
-- | Retrieve a unique 'Ent'.
nextEntity
    :: (Monad m, MonadIO m)
    => SystemT a m Ent
nextEntity = do
  e <- gets _ssNextId
  modify $ ssNextId .~ e + 1
  pure $ Ent e


------------------------------------------------------------------------------
-- | Create a new entity.
createEntity
    :: (HasWorld world m, Monad m)
    => world 'FieldOf
    -> SystemT world m Ent
createEntity cs = do
  e <- nextEntity
  setEntity e $ convertSetter cs
  pure e


------------------------------------------------------------------------------
-- | Delete an entity.
deleteEntity
   :: (HasWorld world m, Monad m)
    => Ent
    -> SystemT world m ()
deleteEntity e = do
  setEntity e delEntity


------------------------------------------------------------------------------
-- | Evaluate a 'QueryT'.
unQueryT
  :: Monad m
  => QueryT world m a
  -> Ent
  -> SystemState world
  -> m (Maybe a)
unQueryT q e f = runMaybeT $ flip R.runReaderT (e, f) $ runQuery' q
{-# INLINE unQueryT #-}


------------------------------------------------------------------------------
-- | Map a 'QueryT' transformation over all entites that match it.
emap
    :: ( HasWorld world m
       , Monad m
       )
    => EntTarget world
    -> QueryT world m (world 'SetterOf)
    -> SystemT world m ()
emap t f = do
  world <- gets id
  let es = t world
  for_ es $ \e -> do
    cs <- gets id
    sets <- lift $ unQueryT f e cs
    for_ sets $ setEntity e
{-# INLINEABLE emap #-}


------------------------------------------------------------------------------
-- | Run a set of separate queries over each entity that matchets the
-- 'EntTarget'. This is asymptoically faster than calling 'emap' for each query
-- individually.
equeryset
    :: ( HasWorld world m
       , Monad m
       , Foldable t
       )
    => EntTarget world
    -> t (QueryT world m (world 'SetterOf))
    -> SystemT world m ()
equeryset t qset = do
  world <- gets id
  let es = t world
  for_ es $ \e -> do
    runQuerySet e qset


runQuerySet
    :: ( HasWorld world m
       , Monad m
       , Foldable t
       )
    => Ent
    -> t (QueryT world m (world 'SetterOf))
    -> SystemT world m ()
runQuerySet ent qset = do
  w  <- gets id
  SystemState _ w' <-
    flip appKEndo w $ flip foldMap qset $ \q -> KEndo $ \world -> do
      sets <- lift $ unQueryT q ent world
      pure $ case sets of
        Nothing -> world
        Just setter ->
          world & ssWorld %~ updateWorld ent setter
  modify $ ssWorld .~ w'


------------------------------------------------------------------------------
-- | Collect the results of a monadic computation over every entity matching
-- a 'QueryT'.
efor
    :: ( HasWorld world m
       , Monad m
       , MonadIO m
       )
    => EntTarget world
    -> QueryT world m a
    -> SystemT world m [a]
efor t f = do
  world <- gets id
  let es = t world
  fmap catMaybes $ for es $ \e -> do
    cs <- gets id
    lift $ unQueryT f e cs
{-# INLINEABLE efor #-}


------------------------------------------------------------------------------
-- | Run a 'QueryT' over a particular 'Ent'.
runQueryT
    :: ( HasWorld world m
       , Monad m
       , MonadIO m
       )
    => Ent
    -> QueryT world m a
    -> SystemT world m (Maybe a)
runQueryT e qt = do
  cs <- gets id
  lift $ unQueryT qt e cs
{-# INLINEABLE runQueryT #-}


getWorld :: (MonadIO m, Monad m) => SystemT world m (world 'WorldOf)
getWorld = gets _ssWorld


------------------------------------------------------------------------------
-- | Provides a resumable 'SystemT'. This is a pretty big hack until I come up
-- with a better formalization for everything.
yieldSystemT
    :: (MonadIO m, Monad m)
    => SystemState world
    -> SystemT world m a
    -> m (SystemState world, a)
yieldSystemT ss m = do
  ref <- liftIO $ newIORef ss
  a <- R.runReaderT (runSystemT' m) ref
  ss' <- liftIO $ readIORef ref
  pure (ss', a)
{-# INLINEABLE yieldSystemT #-}


------------------------------------------------------------------------------
-- | Evaluate a 'SystemT'.
runSystemT
    :: (Monad m, MonadIO m)
    => world 'WorldOf
    -> SystemT world m a
    -> m a
runSystemT w m = do
  ref <- liftIO . newIORef $ SystemState 0 w
  R.runReaderT (runSystemT' m) ref


------------------------------------------------------------------------------
-- | Only evaluate this 'QueryT' for entities which have the given component.
with
    :: forall c a m world
     . ( GetField c
       , IsInjective c a
       , Monad m
       )
    => (world 'WorldOf -> Component 'WorldOf c a)
    -> QueryT world m ()
with = void . query @c @a
{-# INLINEABLE with #-}


------------------------------------------------------------------------------
-- | Only evaluate this 'QueryT' for entities which do not have the given
-- component.
without
    :: forall c a m world
     . ( GetField c
       , IsInjective c a
       , Monad m
       )
    => (world 'WorldOf -> Component 'WorldOf c a)
    -> QueryT world m ()
without f = queryMaybe @c @a f >>= maybe (pure ()) (const empty)
{-# INLINEABLE without #-}


------------------------------------------------------------------------------
-- | Get the value of a component, failing the 'QueryT' if it isn't present.
query
    :: forall c a m world
     . ( GetField c
       , IsInjective c a
       , Monad m
       )
    => (world 'WorldOf -> Component 'WorldOf c a)
    -> QueryT world m a
query f = queryMaybe f >>= maybe empty pure
{-# INLINEABLE query #-}


queryTarget
    :: Monad m
    => EntTarget world
    -> QueryT world m [Ent]
queryTarget t = do
  (_, w) <- QueryT R.ask
  pure $ t w
{-# INLINEABLE queryTarget #-}

------------------------------------------------------------------------------
-- | Run a subquery inside of a 'QueryT'.
subquery
    :: Monad m
    => EntTarget world
    -> QueryT world m a
    -> QueryT world m [a]
subquery t q = do
  (_, w) <- QueryT R.ask
  let es = t w
  lift $ fmap catMaybes $ for es $ \e -> unQueryT q e w
{-# INLINEABLE subquery #-}


------------------------------------------------------------------------------
-- | Attempt to get the value of a component.
queryMaybe
    :: forall c a m world
     . ( GetField c
       , IsInjective c a
       , Monad m
       )
    => (world 'WorldOf -> Component 'WorldOf c a)
    -> QueryT world m (Maybe a)
queryMaybe f = do
  (Ent e, w) <- QueryT R.ask
  pure $ getField @c (f $ _ssWorld w) e
{-# INLINEABLE queryMaybe #-}


------------------------------------------------------------------------------
-- | Attempt to get the owner and value of a unique component.
queryUnique
    :: Monad m
    => (world 'WorldOf -> Component 'WorldOf 'Unique a)
    -> QueryT world m (Maybe (Ent, a))
queryUnique f = do
  (_, w) <- QueryT R.ask
  pure $ coerce $ f $ _ssWorld w
{-# INLINEABLE queryUnique #-}


------------------------------------------------------------------------------
-- | Get the 'Ent' for whom this query is running.
queryEnt
    :: Monad m
    => QueryT world m Ent
queryEnt = QueryT $ R.asks fst
{-# INLINEABLE queryEnt #-}


------------------------------------------------------------------------------
-- | QueryT a flag as a 'Bool'.
queryFlag
    :: forall c m world
     . ( GetField c
       , IsInjective c ()
       , Monad m
       )
    => (world 'WorldOf -> Component 'WorldOf c ())
    -> QueryT world m Bool
queryFlag = fmap (maybe False (const True)) . queryMaybe @c
{-# INLINEABLE queryFlag #-}


------------------------------------------------------------------------------
-- | Perform a query with a default.
queryDef
    :: forall c a m world.
       ( GetField c
       , IsInjective c a
       , Monad m
       )
    => a
    -> (world 'WorldOf -> Component 'WorldOf c a)
    -> QueryT world m a
queryDef z = fmap (maybe z id) . queryMaybe @c
{-# INLINEABLE queryDef #-}


------------------------------------------------------------------------------
-- | An 'EntTarget' is a set of 'Ent's to iterate over.
type EntTarget world = SystemState world -> [Ent]


------------------------------------------------------------------------------
-- | Lifted 'gets' for 'SystemT'.
gets
    :: (Monad m, MonadIO m)
    => (SystemState world -> a)
    -> SystemT world m a
gets f = do
  ref <- SystemT R.ask
  ss <- liftIO $ readIORef ref
  pure $ f ss


------------------------------------------------------------------------------
-- | Lifted 'modify' for 'SystemT'.
modify
    :: (Monad m, MonadIO m)
    => (SystemState world -> SystemState world)
    -> SystemT world m ()
modify f = do
  ref <- SystemT R.ask
  liftIO $ modifyIORef' ref f


------------------------------------------------------------------------------
-- | Iterate over all entities.
allEnts :: EntTarget world
allEnts world = coerce [0 .. _ssNextId world - 1]


entsWith
  :: (world 'WorldOf -> Component 'WorldOf 'Field a)
  -> EntTarget world
entsWith f = coerce . IM.keys . f . _ssWorld


------------------------------------------------------------------------------
-- | Target the entity uniquely identified by owning a 'Unique' field.
uniqueEnt
  :: (world 'WorldOf -> Component 'WorldOf 'Unique a)
  -> EntTarget world
uniqueEnt f = maybe [] (pure . coerce . fst) . f . _ssWorld


------------------------------------------------------------------------------
-- | Iterate over some entities.
someEnts :: [Ent] -> EntTarget world
someEnts = pure


------------------------------------------------------------------------------
-- | Iterate over an entity.
anEnt :: Ent -> EntTarget world
anEnt = pure . pure


------------------------------------------------------------------------------
-- | Turn a 'Maybe' into an 'Update'.
maybeToUpdate :: Maybe a -> Update a
maybeToUpdate Nothing  = Unset
maybeToUpdate (Just a) = Set a


----------------------------------------------------------------------------
-- | Updates an 'Ent' in the world given its setter.
setEntity
    :: ( HasWorld' world
       , Monad m
       , MonadIO m
       )
    => Ent
    -> world 'SetterOf
    -> SystemT world m ()
setEntity e s = do
  w <- getWorld
  modify $ ssWorld .~ updateWorld e s w
{-# INLINEABLE setEntity #-}

