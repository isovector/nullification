module Game.UI where

import qualified Control.Monad.Trans.Reader as R
import           Data.Ecstasy.Types
import           Scripts


getQueuedTransmission :: Game (Maybe (Person, String))
getQueuedTransmission = do
  lgs_ref <- SystemT $ lift $ lift $ R.ask
  lgs <- liftIO $ readIORef lgs_ref
  pure $ listToMaybe $ lgsTransmissionQueue lgs


dequeueTransmission :: Game ()
dequeueTransmission = do
  lgs_ref <- SystemT $ lift $ lift $ R.ask
  liftIO $ modifyIORef' lgs_ref $ field @"lgsTransmissionQueue" %~ drop 1


playQueuedTransmissions :: Game ()
playQueuedTransmissions = do
  getQueuedTransmission >>= \case
    Nothing -> pure ()
    Just (person, msg) -> do
      transmissions <- efor (entsWith eSpecialThing) $ do
        Transmission _ _ <- query eSpecialThing
        pure ()
      when (null transmissions) $ do
        dequeueTransmission
        void $ script_internal_startTransmission person msg
