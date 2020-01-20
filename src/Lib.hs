module Lib (main) where

import Control.FRPNow hiding (when, first)
import Data.Ecstasy.Types
import Game.Sequoia.Color
import Game.Sequoia.Keyboard
import Game.Sequoia.Time
import Interactions
import Prelude hiding (init)
import Drawing
import Control.Monad.Trans.Writer.CPS
import Scripts
import Actions


updateGame :: Time -> V2 -> Game ()
updateGame dt input = do
  emap allEnts $ interact_runScript dt
  emap allEnts $ interact_velToPos dt
  emap allEnts $ interact_accToVel dt
  emap allEnts $ interact_controlledByPlayer input

  lasers <- efor allEnts $ do
    pos <- query ePos
    team <- queryMaybe eTeam
    dir <- queryDef 0 eDirection
    (laser, action) <- query eLaser
    pure $ LaserInteraction pos dir team laser action
  emap allEnts $ interact_laserDamage lasers


initialize :: Game ()
initialize = void $ do
  _player <- createEntity newEntity
    { ePos = Just $ V2 20 250
    , eVel = Just $ V2 100 0
    , eGfx = Just $ do
        pos <- query ePos
        pure $ move pos $ filled grey $ rect 10 10
    , eHurtboxes = Just [Rectangle (V2 (-5) (-5)) $ V2 10 10]
    , eControlled = Just ()
    , eSpeed = Just 100
    , eScript = Just action_blink
    , eTeam = Just PlayerTeam
    }

  void $ createEntity newEntity
    { ePos = Just $ V2 400 300
    , eGfx = Just $ do
        pos <- query ePos
        pure $ move pos $ filled red $ circle 10
    , eHurtboxes = Just [Rectangle (V2 (-10) (-10)) $ V2 20 20]
    , eLaser = Just
        ( LaserRelPos $ V2 0 100
        , pure delEntity
        )
    , eDirection = Just 0
    , eScript = Just $ mconcat
        [ forever $ script_rotate (Radians 2) 1
        , do
            script_goTo (V2 0 0) 100 20
            script_die
        ]
    , eTeam = Just EnemyTeam
    }


runCommand :: Command -> Game ()
runCommand (Spawn proto) = void $ createEntity proto
runCommand (Edit ent proto) = setEntity ent proto


run :: N (B Element)
run = do
  clock <- deltaTime <$> getClock

  keyboard     <- getKeyboard
  -- old_keyboard <- sample $ delayTime clock [] keyboard

  (init, init_cmds) <- liftIO $ fmap (first fst) $ runWriterT $ yieldSystemT (SystemState 0 defStorage defHooks) initialize
  !_ <- unless (null init_cmds) $ error "initialize ran a command!"


  (game, _) <- foldmp init $ \state -> do
    arrs   <- sample $ arrows keyboard
    dt     <- sample clock
    -- kb     <- sample keyboard

    -- old_kb <- sample old_keyboard
    (state', cmds) <- liftIO $ fmap (first fst) $ runWriterT $ yieldSystemT state $ updateGame dt arrs
    (state'', cmds') <- liftIO $ fmap (first fst) $ runWriterT $ yieldSystemT state' $ traverse_ runCommand cmds
    -- DEBUG
    !_ <- unless (null cmds') $ error "runCommand ran a command!"

    pure state''


  poll $ do
    state <- sample game
    liftIO . fmap (collage gameWidth gameHeight . snd . fst)
           . runWriterT
           . yieldSystemT state
           . fmap join
           $ traverse (efor allEnts) drawGame


main :: IO ()
main = play config (const run) pure
  where
    config = EngineConfig (gameWidth, gameHeight) "Nullification" black


gameWidth :: Num a => a
gameWidth = 800

gameHeight :: Num a => a
gameHeight = 600

