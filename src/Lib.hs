module Lib where

import           Abilities
import           Assets
import           Constants
import qualified Control.Monad.Trans.Reader as R
import           Data.Ecstasy.Types
import qualified Data.Map as M
import           Game.Sequoia.Color
import           Game.Sequoia.Keyboard
import           Game.Sequoia.Time
import           GameData
import           Interactions
import           Prelude hiding (init)
import           SDL (SDLException ())
import qualified SDL.Mixer as SDL
import           Tasks


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
  emap (entsWith ePlaySfx)    $ interact_sfxs
  emap (uniqueEnt eIsCamera)  $ interact_focusCamera

  lasers <- efor (entsWith eLaser) $ do
    pos  <- interact_posIfOnScreen
    team <- queryMaybe eTeam
    dir  <- queryDef 0 eDirection
    (laser, action) <- query eLaser
    pure $ LaserInteraction pos dir team laser action
  emap (entsWith eHurtboxes) $ interact_laserDamage dt lasers


  hitboxes <- efor (entsWith eHitboxes) $
    (,,,,)
      <$> interact_posIfOnScreen
      <*> queryMaybe eTeam
      <*> queryEnt
      <*> queryFlag eDieOnContact
      <*> query eHitboxes
  emap (entsWith eHurtboxes) $ interact_hitbox hitboxes



initialize :: Game ()
initialize = void $ do
  startGlobalScript $ doConversation
    [ (esra, "Welcome to the game!")
    , (Person "Karfrew" "man2", "Roger that. Over and out.")
    ]

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


esra :: Person
esra = Person "Esra" "woman6"


doTransmission :: CanRunCommands m => Person -> String -> m Time
doTransmission person msg = do
  let wordcount = fromIntegral $ length $ words msg
      charcount = fromIntegral $ length msg
      wanted_time = wordcount / readingSpeedWordsPerSecond
                  + charcount / characterDisplayPerSecond
      time = max minTransmissionTime wanted_time
  command $ Spawn newEntity
    { eSpecialThing = Just $ Transmission person msg
    , eLifetime     = Just time
    , eAge          = Just 0
    }
  pure time


doConversation :: [(Person, String)] -> Task ()
doConversation [] = pure ()
doConversation ((person, msg) : convo) = do
  time <- doTransmission person msg
  sleep $ time + betweenTransmissionsTime
  doConversation convo


resetGame :: Game ()
resetGame = do
  ref <- SystemT R.ask
  liftIO $ writeIORef ref $ SystemState 0 defStorage defHooks
  initialize

