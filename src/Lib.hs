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


runPlayerScript :: Query () -> Game ()
runPlayerScript t = void $ efor (entsWith eControlled) t


updateGame :: (Key -> Bool) -> Time -> V2 -> Game ()
updateGame is_down dt input = do
  traverse_ (\(key, script) ->
                when (is_down key) $
                  runPlayerScript script)
    [ (EKey, action_blink)
    , (ZKey, action_stop)
    ]

  emap (entsWith eAge)        $ interact_age dt
  emap (entsWith eScript)     $ interact_runScript dt
  emap (entsWith eVel)        $ interact_velToPos dt
  emap (entsWith eAcc)        $ interact_accToVel dt
  emap (entsWith eControlled) $ interact_controlledByPlayer dt input
  emap (uniqueEnt eIsCamera)  $ interact_focusCamera

  lasers <- efor allEnts $ do
    pos <- query ePos
    team <- queryMaybe eTeam
    dir <- queryDef 0 eDirection
    (laser, action) <- query eLaser
    pure $ LaserInteraction pos dir team laser action
  emap (entsWith eHurtboxes) $ interact_laserDamage lasers


  hitboxes <- efor allEnts $ (,,) <$> query ePos <*> queryMaybe eTeam <*> query eHitboxes
  emap (entsWith eHurtboxes) $ interact_hitbox hitboxes



initialize :: Game ()
initialize = void $ do
  let cameraProto = newEntity
        { ePos      = Just $ V2 0 0
        , eIsCamera = Just ()
        }
  void $ createEntity cameraProto

  player <- createEntity newEntity
    { ePos = Just $ V2 20 250
    , eDirection = Just $ Radians pi
    , eVel = Just $ V2 100 0
    , eGfx = Just $ do
        pos <- query ePos
        Radians dir <- query eDirection
        pure $ rotate (dir - pi / 2) $ move (V2 (-16) (-27)) $ toForm $ image "assets/ship.png"
    , eHurtboxes  = Just [Rectangle (V2 (-16) (-16)) $ V2 32 32]
    , eControlled = Just ()
    , eSpeed      = Just 100
    , eTeam       = Just PlayerTeam
    , eFocused    = Just ()
    }

  void $ createEntity newEntity
    { ePos = Just $ V2 400 300
    , eGfx = Just $ do
        pos <- query ePos
        pure $ filled red $ circle 10
    , eHurtboxes = Just [Rectangle (V2 (-10) (-10)) $ V2 20 20]
    , eLaser = Just
        ( LaserRelPos $ V2 0 100
        , pure delEntity
        )
    , eDirection = Just 0
    , eScript = Just $ mconcat
        [ forever $ script_rotate (Radians 2) 1
        ]
    , eTeam = Just EnemyTeam
    }

  void $ createEntity newEntity
    { ePos = Just $ V2 400 550
    , eGfx = Just $ do
        pos <- query ePos
        pure $ filled red $ circle 10
    , eScript = Just $ mconcat
        [ forever $ do
            sleep 2
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
  old_keyboard <- sample $ delayTime clock [] keyboard

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
    kb     <- sample keyboard
    old_kb <- sample old_keyboard
    let is_down k = elem k kb && not (elem k old_kb)

    (state', cmds) <-
      liftIO
        . fmap (first fst)
        . runWriterT
        . yieldSystemT state
        $ updateGame is_down dt arrs
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
      ((_, cameras), _) <-
        runWriterT
          $ yieldSystemT state
          $ efor (uniqueEnt eIsCamera)
          $ query ePos
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

