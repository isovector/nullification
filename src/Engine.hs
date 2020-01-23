module Engine (main) where

import           Constants
import           Control.FRPNow hiding (when, first)
import           Control.Monad.Trans.Writer.CPS
import           Data.Ecstasy.Types
import           Drawing
import qualified Game.Sequoia as S
import           Game.Sequoia.Color
import           Game.Sequoia.Keyboard
import           Game.Sequoia.Time
import           Lib
import           Prelude hiding (init)
import qualified SDL.Mixer as SDL


main :: IO ()
main
  = SDL.withAudio
        (SDL.Audio 44100 SDL.FormatS16_Sys SDL.Stereo)
        4096 $ do
      SDL.setChannels 200
      S.play
          (EngineConfig (gameWidth, gameHeight) "Nullification" black)
          (const run)
          pure

getKeystate :: Bool -> Bool -> Keystate
getKeystate False False = Up
getKeystate False True  = Press
getKeystate True  True  = Down
getKeystate True  False = Unpress

------------------------------------------------------------------------------
-- | Do a game loop; run any resulting commands
execGame
  :: MonadIO m
  => SystemState EntWorld UnderlyingMonad
  -> SystemT EntWorld UnderlyingMonad a
  -> m (SystemState EntWorld UnderlyingMonad)
execGame state m = do
  let runIt s = liftIO . fmap (first fst) . runWriterT . yieldSystemT s
  (state', cmds)   <- runIt state m
  (state'', cmds') <- runIt state' $ traverse_ runCommand cmds
  !_ <- unless (null cmds') $ error "runCommand ran a command!"
  pure state''


------------------------------------------------------------------------------
-- | Run enough of the Game monad to do something pure -- usually for drawing
evalGame
    :: SystemState EntWorld UnderlyingMonad
    -> Game a
    -> IO a
evalGame state m = do
  ((_, a), cmds) <-
    runWriterT $ yieldSystemT state m
  !_ <- unless (null cmds) $ error "evalGame ran a command!"
  pure a


run :: N (B Element)
run = do
  clock        <- deltaTime <$> getClock
  keyboard     <- getKeyboard
  old_keyboard <- sample $ delayTime clock [] keyboard

  init <- execGame (SystemState 0 defStorage defHooks) initialize
  (game, _) <- foldmp init $ \state -> do
    arrs   <- sample $ arrows keyboard
    dt     <- sample clock
    kb     <- sample keyboard
    old_kb <- sample old_keyboard
    let keystate k = getKeystate (elem k old_kb) $ elem k kb
    execGame state $ updateGame keystate dt arrs

  poll $ do
    fps   <- fmap (1 /) $ sample clock
    state <- sample game
    liftIO $ do
      gfx <- evalGame state $ do
        cameras <- efor (uniqueEnt eIsCamera) $ query ePos
        let camera = (fromMaybe 0 $ listToMaybe cameras)
                   - V2 gameWidth gameHeight ^* 0.5
        sequenceA $ draw_game (round fps) camera
      pure $ collage gameWidth gameHeight gfx

