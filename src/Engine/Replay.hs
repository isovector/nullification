module Engine.Replay where

import           Data.Binary (encodeFile, decodeFile)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Game.Sequoia.Keyboard
import           Game.Sequoia.Time
import           Prelude hiding (init)


withRecording
  :: Bool
  -> ([FramePlaybackInfo] -> IORef [FramePlaybackInfo] -> IO ())
  -> IO ()
withRecording playback_last_game f = do
  last_game <-
    bool
      (pure [])
      (decodeFile "/tmp/game.nullification")
      playback_last_game
  recorded <- newIORef []
  f last_game recorded
  result <- readIORef recorded
  encodeFile "/tmp/game.nullification" $ reverse result


makeFramePlaybackInfo :: S.Set Key -> S.Set Key -> Time -> FramePlaybackInfo
makeFramePlaybackInfo old_kb kb dt =
  let set_difference b s1 s2 =
        M.fromList . fmap (flip (,) b) $ S.toList $ s1 S.\\ s2
      !keys_down = set_difference True kb old_kb
      !keys_up   = set_difference False old_kb kb
      !result    = FramePlaybackInfo dt $ keys_down <> keys_up
   in seq result result


recordFramePlaybackInfo
    :: MonadIO m
    => IORef [FramePlaybackInfo]
    -> FramePlaybackInfo
    -> m ()
recordFramePlaybackInfo recorded info =
  liftIO $ modifyIORef' recorded (info :)


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

