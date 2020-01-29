module Lib where

import           Assets
import           Constants
import qualified Control.Monad.Trans.Reader as R
import           Data.Ecstasy.Types
import qualified Data.IntMap as IM
import qualified Data.Map as M
import           Interactions
import           Prelude hiding (init)
import           SDL (SDLException ())
import qualified SDL.Mixer as SDL
import           Scripts


runPlayerScript :: Ent -> Query () -> Game ()
runPlayerScript ent = void . efor (anEnt ent)


runController
    :: (Key -> Keystate)
    -> ControlMapping
    -> Ent
    -> Controller
    -> Game ()
runController keystate mapping ent controller =
  void $ flip M.traverseWithKey controller $ \ctrl ability ->
    for_ (M.lookup ctrl mapping) $ \key ->
      runPlayerScript ent $
        case keystate key of
          Press   -> abilityPress   ability
          Down    -> abilityDown    ability
          Unpress -> abilityUnpress ability
          Up      -> abilityUp      ability


defaultMapping :: ControlMapping
defaultMapping = M.fromList
  [ (Weapon1, SpaceKey)
  , (Weapon2, EKey)
  , (Weapon3, FKey)
  , (Boost,   LeftShiftKey)
  , (Stop,    ZKey)
  ]


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



optimizedEmap
     :: (EntWorld 'WorldOf -> IM.IntMap a)
     -> Query (EntWorld 'SetterOf)
     -> SystemT EntWorld UnderlyingMonad ()
optimizedEmap target = emap $ entsWith target


dispatchController
    :: (Key -> Keystate)
     -> ControlMapping
     -> Game ()
dispatchController keystate mapping = do
  controlled <-
    efor (entsWith eControlled) $
      (,)
        <$> queryEnt
        <*> query eAbilities
  for_ controlled $ \(ent, controller) ->
    runController keystate mapping ent controller


updateGame :: (Key -> Keystate) -> Time -> V2 -> Game ()
updateGame keystate dt input = do
  when (keystate RKey == Press && keystate LeftControlKey == Down) resetGame

  let mapping = defaultMapping
  dispatchController keystate mapping

  playQueuedTransmissions

  optimizedEmap eAge        $ interact_age dt
  optimizedEmap eLifetime   $ interact_lifetime dt
  optimizedEmap eHitpoints  $ interact_manageHitpoints
  optimizedEmap eScript     $ interact_runScript dt
  optimizedEmap eVel        $ interact_velToPos dt
  optimizedEmap eDragRate   $ interact_drag dt
  optimizedEmap eAcc        $ interact_accToVel dt
  optimizedEmap eControlled $ interact_controlledByPlayer dt input
  optimizedEmap ePlaySfx    $ interact_sfxs
  emap (uniqueEnt eIsCamera)  $ interact_focusCamera

  lasers <- efor (entsWith eLaser) $ do
    interact_onlyIfAlive
    pos  <- interact_posIfOnScreen
    team <- queryMaybe eTeam
    dir  <- queryDef 0 eDirection
    (laser, action) <- query eLaser
    pure $ LaserInteraction pos dir team laser action
  emap (entsWith eHurtboxes) $ interact_laserDamage dt lasers


  hitboxes <- efor (entsWith eHitboxes) $ do
    interact_onlyIfAlive
    (,,,,)
      <$> interact_posIfOnScreen
      <*> queryMaybe eTeam
      <*> queryEnt
      <*> queryFlag eDieOnContact
      <*> query eHitboxes
  emap (entsWith eHurtboxes) $ interact_hitbox hitboxes

  emap (entsWith eDeathState) $ interact_waitForDeathScript
  emap (entsWith eDeathState) $ interact_startDeathScript


runCommand :: Command -> Game ()
runCommand (Spawn proto) = void $ createEntity proto
runCommand (Edit ent proto) = setEntity ent proto
runCommand (Sfx sfx) =
  liftIO $ void $ try @SDLException $ SDL.play $ sfx soundBank
runCommand (Transmit person msg) = do
  lgs_ref <- SystemT $ lift $ lift $ R.ask
  liftIO $ modifyIORef' lgs_ref $
    field @"lgsTransmissionQueue" <>~ [(person, msg)]


resetGame :: Game ()
resetGame = do
  lgs_ref <- SystemT $ lift $ lift $ R.ask
  LocalGameState level _ <- liftIO $ readIORef lgs_ref
  system_state_ref <- SystemT R.ask

  liftIO $ do
    writeIORef system_state_ref $ SystemState 0 defStorage
    writeIORef lgs_ref          $ LocalGameState level []
  level


getQueuedTransmission :: Game (Maybe (Person, String))
getQueuedTransmission = do
  lgs_ref <- SystemT $ lift $ lift $ R.ask
  lgs <- liftIO $ readIORef lgs_ref
  pure $ listToMaybe $ lgsTransmissionQueue lgs

dequeueTransmission :: Game ()
dequeueTransmission = do
  lgs_ref <- SystemT $ lift $ lift $ R.ask
  liftIO $ modifyIORef' lgs_ref $ field @"lgsTransmissionQueue" %~ drop 1


getCameraPos :: Game V2
getCameraPos = do
  cameras <- efor (uniqueEnt eIsCamera) $ query ePos
  pure $ (fromMaybe 0 $ listToMaybe cameras)
       - V2 gameWidth gameHeight ^* 0.5

