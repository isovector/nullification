module Engine.GameMonad where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.CPS
import Lib
import Prelude hiding (init)


------------------------------------------------------------------------------
-- | Do a game loop; engine_main any resulting commands
execGame
  :: MonadIO m
  => IORef LocalGameState
  -> SystemState EntWorld
  -> SystemT EntWorld UnderlyingMonad ()
  -> m (SystemState EntWorld)
execGame lgs state m = do
  let runIt s = liftIO
              . fmap (first fst)
              . flip runReaderT lgs
              . runWriterT
              . yieldSystemT s
  (state', cmds)   <- runIt state m
  (state'', cmds') <- runIt state' $ traverse_ runCommand cmds
  !_ <- unless (null cmds') $ error "runCommand ran a command!"
  pure state''


------------------------------------------------------------------------------
-- | Run enough of the Game monad to do something pure -- usually for drawing
evalGame
    :: IORef LocalGameState
    -> SystemState EntWorld
    -> Game a
    -> IO a
evalGame lgs state m = do
  ((_, a), cmds) <- flip runReaderT lgs $ runWriterT $ yieldSystemT state m
  !_ <- unless (null cmds) $ error "evalGame ran a command!"
  pure a
