module Lib where

import           Assets
import           Constants
import qualified Control.Monad.Trans.Reader as R
import           Data.Ecstasy.Types
import qualified Data.IntMap as IM
import           Game.Combat
import           Game.Input
import           Game.UI
import           Interactions
import           Prelude hiding (init)
import           SDL (SDLException ())
import qualified SDL.Mixer as SDL




game_updateControl
    :: (Key -> Keystate)
    -> ControlMapping
    -> Time
    -> V2
    -> Game ()
game_updateControl keystate mapping dt input = do
  controlled <-
    efor (entsWith eControlled) $
      (,)
        <$> queryEnt
        <*> query eAbilities
  for_ controlled $ \(ent, controller) ->
    runController keystate mapping ent controller

  optimizedEmap eControlled $ interact_controlledByPlayer dt input


updateGame :: (Key -> Keystate) -> Time -> V2 -> Game ()
updateGame keystate dt input = do
  game_debugCommands keystate
  game_updateControl keystate defaultMapping dt input
  game_updateUI
  game_updateScripts dt
  game_updatePhysics dt


game_debugCommands :: (Key -> Keystate) -> Game ()
game_debugCommands keystate =
  when (keystate RKey == Press && keystate LeftControlKey == Down) resetGame


game_updateUI :: Game ()
game_updateUI = do
  playQueuedTransmissions
  emap (uniqueEnt eIsCamera) $ interact_focusCamera


game_updatePhysics :: Time -> Game ()
game_updatePhysics dt = do
  optimizedEmap eVel        $ interact_velToPos dt
  optimizedEmap eDragRate   $ interact_drag dt
  optimizedEmap eAcc        $ interact_accToVel dt


game_updateScripts :: Time -> Game ()
game_updateScripts dt = do
  optimizedEmap eAge       $ interact_age dt
  optimizedEmap eLifetime  $ interact_lifetime dt
  optimizedEmap eScript    $ interact_runScript dt
  optimizedEmap ePlaySfx   $ interact_sfxs


game_updateCombat :: Time -> Game ()
game_updateCombat dt = do
  runLasers dt
  runHitboxes

  optimizedEmap eHitpoints  $ interact_manageHitpoints
  optimizedEmap eDeathState $ interact_waitForDeathScript
  optimizedEmap eDeathState $ interact_startDeathScript


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


getCameraPos :: Game V2
getCameraPos = do
  cameras <- efor (uniqueEnt eIsCamera) $ query ePos
  pure $ (fromMaybe 0 $ listToMaybe cameras)
       - V2 gameWidth gameHeight ^* 0.5


optimizedEmap
     :: (EntWorld 'WorldOf -> IM.IntMap a)
     -> Query (EntWorld 'SetterOf)
     -> SystemT EntWorld UnderlyingMonad ()
optimizedEmap target = emap $ entsWith target

