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
import GameData
import Tasks


updateGame :: Time -> V2 -> Game ()
updateGame dt input = do
  traverse_ (emap allEnts)
    [ interact_age dt
    , interact_runScript dt
    , interact_velToPos dt
    , interact_accToVel dt
    , interact_controlledByPlayer input
    ]

  lasers <- efor allEnts $ do
    pos <- query ePos
    team <- queryMaybe eTeam
    dir <- queryDef 0 eDirection
    (laser, action) <- query eLaser
    pure $ LaserInteraction pos dir team laser action
  emap allEnts $ interact_laserDamage lasers


initialize :: Game ()
initialize = void $ do
  player <- createEntity newEntity
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
    , eFocused = Just ()
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
        -- , do
        --     script_goTo (V2 0 0) 100 20
        --     script_die
        ]
    , eTeam = Just EnemyTeam
    }

  void $ createEntity newEntity
    { ePos = Just $ V2 400 550
    , eGfx = Just $ do
        pos <- query ePos
        pure $ move pos $ filled red $ circle 10
    , eScript = Just $ mconcat
        [ forever $ do
            sleep 0.2
            action_shootAt 4 gun player
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

  (init, init_cmds) <-
    liftIO
      . fmap (first fst)
      . runWriterT
      . yieldSystemT (SystemState 0 defStorage defHooks)
      $ initialize
  !_ <- unless (null init_cmds) $ error "initialize ran a command!"


  (game, _) <- foldmp init $ \state -> do
    arrs   <- sample $ arrows keyboard
    dt     <- sample clock
    -- kb     <- sample keyboard

    -- old_kb <- sample old_keyboard
    (state', cmds) <-
      liftIO
        . fmap (first fst)
        . runWriterT
        . yieldSystemT state
        $ updateGame dt arrs
    (state'', cmds') <-
      liftIO
        . fmap (first fst)
        . runWriterT
        . yieldSystemT state'
        $ traverse_ runCommand cmds
    -- DEBUG
    !_ <- unless (null cmds') $ error "runCommand ran a command!"

    pure state''


  poll $ do
    state <- sample game
    liftIO $ do
      ((_, cameras), _) <- runWriterT $ yieldSystemT state $ efor allEnts $ do
        with eFocused
        query ePos
      let camera = (fromMaybe 0 $ listToMaybe cameras) - V2 gameWidth gameHeight ^* 0.5
          moveGroup v2 = pure . move v2 . group

      fmap (collage gameWidth gameHeight . moveGroup (-camera) . snd . fst)
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

