{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tasks where

import Control.Monad.Coroutine (resume)
import Control.Monad.Coroutine.SuspensionFunctors


startGlobalScript :: CanRunCommands m => Task () -> m ()
startGlobalScript script =
  command $ Spawn newEntity
    { eScript = Just script
    }


sleep :: Time -> Task ()
sleep time | time <= 0 = pure ()
sleep time = do
  dt <- request unchanged
  sleep $ time - dt


timeTask :: Task a -> Task (Time, a)
timeTask = timer' 0
  where
    timer' time task = do
      lift (resume task) >>= \case
        Left (Request e t') -> do
          dt <- request e
          timer' (time + dt) $ t' dt
        Right a -> pure (time, a)


over :: Time -> (Progress -> Task ()) -> Task ()
over t = over' t t
  where
    over' _ time _ | time <= 0 = pure ()
    over' total time f = do
      (dt, _) <- timeTask $ f (Progress $ 1 - (time / total))
      over' total (time - dt) f


interleave :: Task () -> Task () -> Task ()
interleave t1 t2 =
  lift (resume t1) >>= \case
    Left (Request e1 t1') ->
      lift (resume t2) >>= \case
        Left (Request e2 t2') -> do
          dt1 <- request e1
          dt2 <- request e2
          let dt' = dt1 + dt2
          interleave (t1' dt') (t2' dt')
        Right () -> request e1 >>= t1'
    Right () ->
      lift (resume t2) >>= \case
        Left (Request e2 t2') -> request e2 >>= t2'
        Right () -> pure ()


instance Semigroup (Task ()) where
  (<>) = interleave

instance Monoid (Task ()) where
  mempty = pure ()

