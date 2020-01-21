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
import Actions
import GameData
import qualified Control.Monad.Trans.Reader as R


runPlayerScript :: Query () -> Game ()
runPlayerScript = void . efor (entsWith eControlled)


updateGame :: (Key -> Keystate) -> Time -> V2 -> Game ()
updateGame keystate dt input = do
  traverse_ (\(state, key, script) ->
                when (keystate key == state) $
                  runPlayerScript script)
    [ (Press, EKey, action_blink)
    , (Unpress, EKey, action_blink_unpress)
    , (Press, ZKey, action_stop)
    ]

  when (keystate RKey == Press && keystate LeftShiftKey == Down) resetGame

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


resetGame :: Game ()
resetGame = do
  ref <- SystemT R.ask
  liftIO $ writeIORef ref $ SystemState 0 defStorage defHooks
  initialize


initialize :: Game ()
initialize = void $ do
  let cameraProto = newEntity
        { ePos      = Just $ V2 0 0
        , eIsCamera = Just ()
        }
  void $ createEntity cameraProto

  player <- createEntity newEntity
    { ePos = Just $ V2 512 (-500)
    , eDirection = Just $ Radians $ pi / 2
    , eVel = Just $ V2 0 100
    , eGfx = Just $ do
        Radians dir <- query eDirection
        pure $ rotate dir $ move (V2 (-27) (-16)) $ toForm $ image "assets/ship.png"
    , eHurtboxes  = Just [Rectangle (V2 (-16) (-16)) $ V2 32 32]
    , eControlled = Just ()
    , eSpeed      = Just 100
    , eTeam       = Just PlayerTeam
    , eFocused    = Just ()
    }

  let mkWall x y =
        void $ createEntity wall
          { ePos = Just $ V2 (x * 127) (y * 127)
          }


  for_ [-3..2] $ \x -> mkWall x 0

  traverse_ (uncurry mkWall) $ (,) <$> [-3, 21] <*> [0..10]

  for_ [5..13] $ \x -> mkWall x 0
  for_ [16..21] $ \x -> mkWall x 0

  mkWall (-1) 5
  mkWall 19 5

  for_ [2..7] $ \x -> mkWall x 6
  for_ [11..16] $ \x -> mkWall x 7
  mkWall 2 7
  mkWall 2 8
  mkWall 16 8
  mkWall 16 9

  for_ [2..9] $ \x -> mkWall x 9
  for_ [9..16] $ \x -> mkWall x 10

  let mkTurret x y =
        void $ createEntity (turret player)
          { ePos = Just $ V2 (x * 128 + 64) (y * 128 + 64)
          }

  traverse_ (uncurry mkTurret) $ (,) <$> [1.75, 2.25, 4.75, 5.25] <*> [-1, 1]
  traverse_ (uncurry mkTurret) $ (,) <$> (fmap (11 +) [1.75, 2.25, 4.75, 5.25]) <*> [-1, 1]

  let laserFence src dst = newEntity
        { ePos = Just src
        , eLaser = Just
            ( LaserAbsPos dst
            , pure delEntity
            )
        -- , eTeam = Just EnemyTeam
        }
      mkLaserFence x1 y1 x2 y2 =
        void $ createEntity $
          laserFence (V2 (x1 * 128 + 64) (y1 * 128 + 64))
                     (V2 (x2 * 128 + 64) (y2 * 128 + 64))
  traverse_ (\(x, y) -> mkLaserFence x y (x + 3) y) $ (,) <$> [2, 13] <*> [-0.1, 0.1]




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
    let keystate   k = getKeystate (elem k old_kb) $ elem k kb

    (state', cmds) <-
      liftIO
        . fmap (first fst)
        . runWriterT
        . yieldSystemT state
        $ updateGame keystate dt arrs
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

      ((_, forms), _) <-
        runWriterT
           . yieldSystemT state
           . fmap join
           $ traverse (efor allEnts) drawGame
      pure $ collage gameWidth gameHeight
           . (toForm (image "assets/space.png") :)
           -- . fmap (scale 0.2)
           $ moveGroup (-camera) forms


getKeystate :: Bool -> Bool -> Keystate
getKeystate False False = Up
getKeystate False True = Press
getKeystate True True = Down
getKeystate True False = Unpress


main :: IO ()
main = play config (const run) pure
  where
    config = EngineConfig (gameWidth, gameHeight) "Nullification" black


gameWidth :: Num a => a
gameWidth = 800

gameHeight :: Num a => a
gameHeight = 600

