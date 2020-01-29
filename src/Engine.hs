module Engine (main) where

import           Constants
import           Control.FRPNow hiding (when, first)
import           Data.Ecstasy.Types
import qualified Data.Set as S
import           Drawing
import           Engine.Controller
import           Engine.Replay
import           Engine.GameMonad
import qualified Game.Sequoia as S
import           Game.Sequoia.Color
import           Game.Sequoia.Keyboard
import           Game.Sequoia.Time
import           Level.Fortress (fortress)
import           Level.MainMenu (mainMenu)
import           Lib
import           Prelude hiding (init)
import qualified SDL.Mixer as SDL


startingLevel :: Game ()
startingLevel = fortress


engine_draw
    :: IORef LocalGameState
    -> B Time
    -> B (SystemState EntWorld)
    -> N (B Element)
engine_draw lgs clock game = poll $ do
  fps   <- fmap (1 /) $ sample clock
  state <- sample game
  liftIO $ do
    gfx <- evalGame lgs state $ do
      camera <- getCameraPos
      sequenceA $ draw_game (round fps) camera
    pure $ collage gameWidth gameHeight gfx


engine_update
    :: MonadIO m
    => IORef LocalGameState
    -> IORef [FramePlaybackInfo]
    -> SystemState EntWorld
    -> (Time, S.Set Key, S.Set Key)
    -> m (SystemState EntWorld)
engine_update lgs recorded state (dt, old_kb, kb) = do
  recordFramePlaybackInfo recorded $ makeFramePlaybackInfo old_kb kb dt
  execGame lgs state $
    updateGame
      (buildKeystate old_kb kb)
      dt
      (arrows kb)


main :: IO ()
main = do
  let playback_last_game = False
  withRecording playback_last_game $ \last_game recorded ->
    SDL.withAudio
        (SDL.Audio 44100 SDL.FormatS16_Sys SDL.Stereo)
        4096 $ do
      SDL.setChannels 200
      S.play
        (EngineConfig (gameWidth, gameHeight) "Nullification" black)
        (const $ engine_main last_game recorded)
        pure


engine_main
    :: [FramePlaybackInfo]
    -> IORef [FramePlaybackInfo]
    -> N (B Element)
engine_main playback recorded = do
  lgs    <- liftIO $ newIORef $ LocalGameState startingLevel []
  state0 <- engine_intialize lgs playback recorded
  engine_loop lgs recorded state0 >>= uncurry (engine_draw lgs)


engine_intialize
    :: MonadIO m
    => IORef LocalGameState
    -> [FramePlaybackInfo]
    -> IORef [FramePlaybackInfo]
    -> m (SystemState EntWorld)
engine_intialize lgs playback recorded = do
  init  <- execGame lgs (SystemState 0 defStorage) resetGame
  foldM (engine_update lgs recorded) init $
    unfoldFramePlaybackInfo playback


engine_loop
    :: IORef LocalGameState
    -> IORef [FramePlaybackInfo]
    -> SystemState EntWorld
    -> N (B Time, B (SystemState EntWorld))
engine_loop lgs recorded state0 = do
  clock        <- deltaTime <$> getClock
  keyboard     <- getKeyboard
  old_keyboard <- sample $ delayTime clock mempty keyboard

  (game, _) <- foldmp state0 $ \state -> do
    dt     <- sample clock
    old_kb <- sample old_keyboard
    kb     <- sample keyboard
    engine_update lgs recorded state (dt, old_kb, kb)

  pure (clock, game)

