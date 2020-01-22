module Lib (main) where

import           Abilities
import           Assets
import           Control.FRPNow hiding (when, first)
import qualified Control.Monad.Trans.Reader as R
import           Control.Monad.Trans.Writer.CPS
import           Data.Ecstasy.Types
import qualified Data.Map as M
import           Drawing
import qualified Game.Sequoia as S
import           Game.Sequoia.Color
import           Game.Sequoia.Keyboard
import           Game.Sequoia.Time
import           GameData
import           Interactions
import           Prelude hiding (init)
import           SDL (SDLException ())
import qualified SDL.Mixer as SDL


runPlayerScript :: Ent -> Query () -> Game ()
runPlayerScript ent = void . efor (anEnt ent)


runController
    :: (Key -> Keystate)
    -> ControlMapping
    -> Ent
    -> Controller
    -> Game ()
runController keystate mapping ent controller =
  void $ flip M.traverseWithKey controller $ \ctrl ability ->
    for_ (M.lookup ctrl mapping) $ \key ->
      runPlayerScript ent $
        case keystate key of
          Press   -> abilityPress   ability
          Down    -> abilityDown    ability
          Unpress -> abilityUnpress ability
          Up      -> abilityUp      ability


defaultMapping :: ControlMapping
defaultMapping = M.fromList
  [ (Weapon1, SpaceKey)
  , (Weapon2, EKey)
  , (Weapon3, FKey)
  , (Boost,   LeftShiftKey)
  , (Stop,    ZKey)
  ]


updateGame :: (Key -> Keystate) -> Time -> V2 -> Game ()
updateGame keystate dt input = do
  when (keystate RKey == Press && keystate LeftControlKey == Down) resetGame

  let mapping = defaultMapping
  controlled <-
    efor (entsWith eControlled) $
      (,)
        <$> queryEnt
        <*> query eControlled
  for_ controlled $ \(ent, controller) ->
    runController keystate mapping ent controller

  emap (entsWith eAge)        $ interact_age dt
  emap (entsWith eLifetime)   $ interact_lifetime dt
  emap (entsWith eHitpoints)  $ interact_manageHitpoints
  emap (entsWith eScript)     $ interact_runScript dt
  emap (entsWith eVel)        $ interact_velToPos dt
  emap (entsWith eAcc)        $ interact_accToVel dt
  emap (entsWith eControlled) $ interact_controlledByPlayer dt input
  emap (uniqueEnt eIsCamera)  $ interact_focusCamera

  lasers <- efor (entsWith eLaser) $ do
    pos  <- interact_onlyIfOnScreen
    team <- queryMaybe eTeam
    dir  <- queryDef 0 eDirection
    (laser, action) <- query eLaser
    pure $ LaserInteraction pos dir team laser action
  emap (entsWith eHurtboxes) $ interact_laserDamage dt lasers


  hitboxes <- efor (entsWith eHitboxes) $
    (,,,,)
      <$> interact_onlyIfOnScreen
      <*> queryMaybe eTeam
      <*> queryEnt
      <*> queryFlag eDieOnContact
      <*> query eHitboxes
  emap (entsWith eHurtboxes) $ interact_hitbox hitboxes


minimapScale :: Num a => a
minimapScale = 32


draw_minimapBlip :: Query Form
draw_minimapBlip = do
  pos           <- query ePos
  (color, size) <- query eOnMinimap
  let size' = size * minimapScale
  pure $ move pos $ filled color $ rect size' size'


draw_minimap :: V2 -> Game Form
draw_minimap camera = do
  forms <- efor (entsWith eOnMinimap) draw_minimapBlip
  let minimap = scale (1 / minimapScale) $ move (-camera) $  group forms
  pure minimap



resetGame :: Game ()
resetGame = do
  ref <- SystemT R.ask
  liftIO $ writeIORef ref $ SystemState 0 defStorage defHooks
  initialize


initialize :: Game ()
initialize = void $ do
  let cameraProto = newEntity
        { ePos      = Just $ V2 512 $ -2000
        , eIsCamera = Just ()
        }
  void $ createEntity cameraProto

  void $ createEntity
    ( collectable $ do
        Radians angle <- query eDirection
        pure unchanged
          { eDirection = Set $ Radians $ angle + pi
          }
    ) { ePos = Just $ V2 512 (-900)
      }

  player <- createEntity newEntity
    { ePos = Just $ V2 512 (-2000)
    , eOrigin = Just $ V2 27 16
    , eDirection = Just $ Radians $ pi / 2
    , eVel = Just $ V2 0 100
    , eGfx = Just $ do
        pure $ toForm $ image "assets/ship.png"
    , eHurtboxes  = Just [Rectangle (V2 (-16) (-16)) $ V2 32 32]
    , eControlled = Just $ M.fromList
        [ (Weapon1, ability_multishoot (Radians $ 2 * pi) 16 $ gun 2)
        , (Weapon2, ability_laser 300 10)
        , (Weapon3, ability_multishoot (Radians $ 2 * pi) 6 $ clusterGun 0.5 6 $ gun 2)
        , (Boost,   ability_finalBlink)
        , (Stop,    ability_stop)
        ]
    , eSpeed     = Just 100
    , eTeam      = Just PlayerTeam
    , eFocused   = Just ()
    , eOnMinimap = Just (green, 3)
    , eHitpoints = Just 10
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
          { ePos = Just $ V2 (x * 128) (y * 128)
          }

  traverse_ (uncurry mkTurret) $ (,) <$> [1.75, 2.25, 4.75, 5.25] <*> [-1, 1]
  traverse_ (uncurry mkTurret) $ (,) <$> (fmap (11 +) [1.75, 2.25, 4.75, 5.25]) <*> [-1, 1]

  let laserFence src dst = newEntity
        { ePos = Just src
        , eLaser = Just
            ( LaserAbsPos dst
            , \dt -> interact_damage $ 50 * dt
            )
        -- , eTeam = Just EnemyTeam
        }
      mkLaserFence x1 y1 x2 y2 =
        void $ createEntity $
          laserFence (V2 (x1 * 128) (y1 * 128))
                     (V2 (x2 * 128) (y2 * 128))
  traverse_ (\(x, y) -> mkLaserFence x y (x + 3) y) $ (,) <$> [2, 13] <*> [-0.1, 0.1]




runCommand :: Command -> Game ()
runCommand (Spawn proto) = void $ createEntity proto
runCommand (Edit ent proto) = setEntity ent proto
runCommand (Sfx sfx) = liftIO $ void $ try @SDLException $ SDL.play $ sfx soundBank


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
    fps   <- fmap (1 /) $ sample clock
    state <- sample game
    liftIO $ do
      ((_, cameras), _) <-
        runWriterT
          $ yieldSystemT state
          $ efor (uniqueEnt eIsCamera)
          $ query ePos
      let camera = (fromMaybe 0 $ listToMaybe cameras) - V2 gameWidth gameHeight ^* 0.5
          moveGroup v2 = pure . move v2 . group

      ((_, (forms, minimap)), _) <-
        runWriterT
           . yieldSystemT state
           $ do
        gfxs <- join <$> traverse (efor allEnts) drawGame
        minimap <- draw_minimap camera
        pure (gfxs, minimap)
      pure $ collage gameWidth gameHeight
           . (toForm (image "assets/space.png") :)
           . (++ [ move (V2 32 (600 - 64)) minimap
                 , move 20 $ draw_text $ show @Int $ round fps
                 ])
           -- . fmap (scale 0.2)
           $ moveGroup (-camera) forms


getKeystate :: Bool -> Bool -> Keystate
getKeystate False False = Up
getKeystate False True = Press
getKeystate True True = Down
getKeystate True False = Unpress


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


gameWidth :: Num a => a
gameWidth = 800

gameHeight :: Num a => a
gameHeight = 600

