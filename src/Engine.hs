module Engine (main) where

import           Constants
import           Control.FRPNow hiding (when, first)
import           Control.Monad.Trans.Writer.CPS
import           Data.Binary (encodeFile, decodeFile)
import           Data.Ecstasy.Types
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Drawing
import qualified Game.Sequoia as S
import           Game.Sequoia.Color
import           Game.Sequoia.Keyboard
import           Game.Sequoia.Time
import           Lib
import           Prelude hiding (init)
import qualified SDL.Mixer as SDL
import Control.Monad.Trans.Reader


main :: IO ()
main = do
  let playback_last_game = False

  last_game <-
    bool
      (pure [])
      (decodeFile "/tmp/game.nullification")
      playback_last_game
  recorded <- newIORef []
  SDL.withAudio
        (SDL.Audio 44100 SDL.FormatS16_Sys SDL.Stereo)
        4096 $ do
    SDL.setChannels 200
    S.play
        (EngineConfig (gameWidth, gameHeight) "Nullification" black)
        (const $ run last_game recorded)
        pure
  result <- readIORef recorded
  encodeFile "/tmp/game.nullification" $ reverse result

getKeystate :: Bool -> Bool -> Keystate
getKeystate False False = Up
getKeystate False True  = Press
getKeystate True  True  = Down
getKeystate True  False = Unpress


------------------------------------------------------------------------------
-- | Do a game loop; run any resulting commands
execGame
  :: MonadIO m
  => IORef LocalGameState
  -> SystemState EntWorld UnderlyingMonad
  -> SystemT EntWorld UnderlyingMonad ()
  -> m (SystemState EntWorld UnderlyingMonad)
execGame lgs state m = do
  let runIt s = liftIO . fmap (first fst) . flip runReaderT lgs . runWriterT . yieldSystemT s
  (state', cmds)   <- runIt state m
  (state'', cmds') <- runIt state' $ traverse_ runCommand cmds
  !_ <- unless (null cmds') $ error "runCommand ran a command!"
  pure state''


------------------------------------------------------------------------------
-- | Run enough of the Game monad to do something pure -- usually for drawing
evalGame
    :: IORef LocalGameState
    -> SystemState EntWorld UnderlyingMonad
    -> Game a
    -> IO a
evalGame lgs state m = do
  ((_, a), cmds) <- flip runReaderT lgs $ runWriterT $ yieldSystemT state m
  !_ <- unless (null cmds) $ error "evalGame ran a command!"
  pure a


run :: [FramePlaybackInfo] -> IORef [FramePlaybackInfo] -> N (B Element)
run playback recorded = do
  lgs <- liftIO $ newIORef $ LocalGameState startingLevel []

  init <- execGame lgs (SystemState 0 defStorage defHooks) resetGame
  init' <- foldM (gameFrame lgs recorded) init $ unfoldFramePlaybackInfo playback

  clock        <- deltaTime <$> getClock
  keyboard     <- getKeyboard
  old_keyboard <- sample $ delayTime clock mempty keyboard

  (game, _) <- foldmp init' $ \state -> do
    dt     <- sample clock
    old_kb <- sample old_keyboard
    kb     <- sample keyboard
    gameFrame lgs recorded state (dt, old_kb, kb)

  poll $ do
    fps   <- fmap (1 /) $ sample clock
    state <- sample game
    liftIO $ do
      gfx <- evalGame lgs state $ do
        cameras <- efor (uniqueEnt eIsCamera) $ query ePos
        let camera = (fromMaybe 0 $ listToMaybe cameras)
                   - V2 gameWidth gameHeight ^* 0.5
        sequenceA $ draw_game (round fps) camera
      pure $ collage gameWidth gameHeight gfx


gameFrame
    :: MonadIO m
    => IORef LocalGameState
    -> IORef [FramePlaybackInfo]
    -> SystemState EntWorld UnderlyingMonad
    -> (Time, S.Set Key, S.Set Key)
    -> m (SystemState EntWorld UnderlyingMonad)
gameFrame lgs recorded state (dt, old_kb, kb) = do
  let arrs = arrows kb
  let keystate k = getKeystate (S.member k old_kb) $ S.member k kb
  liftIO $ modifyIORef' recorded (makeFramePlaybackInfo old_kb kb dt :)
  execGame lgs state $ updateGame keystate dt arrs


makeFramePlaybackInfo :: S.Set Key -> S.Set Key -> Time -> FramePlaybackInfo
makeFramePlaybackInfo old_kb kb dt =
  let set_difference b s1 s2 =
        M.fromList . fmap (flip (,) b) $ S.toList $ s1 S.\\ s2
      !keys_down = set_difference True kb old_kb
      !keys_up   = set_difference False old_kb kb
      !result    = FramePlaybackInfo dt $ keys_down <> keys_up
   in seq result result


unfoldFramePlaybackInfo :: [FramePlaybackInfo] -> [(Time, S.Set Key, S.Set Key)]
unfoldFramePlaybackInfo = go mempty
  where
    go _ [] = []
    go old_kb (FramePlaybackInfo dt delta_keys : fpis) =
      let kb =
            flip appEndo old_kb $ flip foldMap (M.assocs delta_keys) $ \case
              (k, True)  -> Endo (S.insert k)
              (k, False) -> Endo (S.delete k)
       in (dt, old_kb, kb) : go kb fpis

