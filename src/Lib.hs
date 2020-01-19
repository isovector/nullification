module Lib (main) where

import Control.FRPNow hiding (when)
import Data.Ecstasy.Types
import Game.Sequoia.Color
import Game.Sequoia.Keyboard
import Game.Sequoia.Time
import Interactions
import Prelude hiding (init)
import Types
import Drawing


updateGame :: Time -> V2 -> Game ()
updateGame dt input = do
  emap allEnts $ interact_rotatingBody dt
  emap allEnts $ interact_velToPos dt
  emap allEnts $ interact_accToVel dt
  emap allEnts $ interact_controlledByPlayer input

  lasers <- efor allEnts $ do
    pos <- query ePos
    team <- queryDef NeutralTeam eTeam
    dir <- queryDef 0 eDirection
    (laser, action) <- query eLaser
    pure $ LaserInteraction pos dir team laser action
  emap allEnts $ interact_laserDamage lasers


initialize :: Game ()
initialize = void $ do
  _player <- createEntity newEntity
    { ePos = Just $ V2 20 250
    , eVel = Just $ V2 0 0
    , eGfx = Just $ do
        pos <- query ePos
        pure $ move pos $ filled grey $ rect 10 10
    , eHurtboxes = Just [Rectangle (V2 (-5) (-5)) $ V2 10 10]
    , eControlled = Just ()
    , eSpeed = Just 100
    , eTeam = Just PlayerTeam
    }

  createEntity newEntity
    { ePos = Just $ V2 400 300
    , eGfx = Just $ do
        pos <- query ePos
        pure $ move pos $ filled red $ circle 10
    , eLaser = Just
        ( LaserRelPos $ V2 0 100
        , pure delEntity
        )
    , eDirection = Just 0
    , eRotationSpeed = Just $ Radians 2
    , eTeam = Just EnemyTeam
    }


run :: N (B Element)
run = do
  clock <- deltaTime <$> getClock

  keyboard     <- getKeyboard
  -- old_keyboard <- sample $ delayTime clock [] keyboard

  init <- liftIO $ fmap fst $ yieldSystemT (SystemState 0 defStorage defHooks) initialize

  (game, _) <- foldmp init $ \state -> do
    arrs   <- sample $ arrows keyboard
    dt     <- sample clock
    kb     <- sample keyboard

    !_ <- when (elem EscapeKey kb) $ error "quit"

    -- old_kb <- sample old_keyboard
    liftIO $ fmap fst $ yieldSystemT state $ updateGame dt arrs

  poll $ do
    state <- sample game
    liftIO $ fmap (collage gameWidth gameHeight . snd) $ yieldSystemT state $ do
      fmap join $ traverse (efor allEnts) drawGame


    -- let cam = _lsCamera $ fst state
    --     mouse' = mouse { mPos = mPos mouse + cam }

    -- liftIO . fmap (collage gameWidth gameHeight)
    --        . evalGame state
    --        $ draw mouse'


main :: IO ()
main = play config (const run) pure
  where
    config = EngineConfig (gameWidth, gameHeight) "Nullification" black


gameWidth :: Num a => a
gameWidth = 800

gameHeight :: Num a => a
gameHeight = 600

