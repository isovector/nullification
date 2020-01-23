module Level.Fortress where

import Constants
import Entity.Camera
import Entity.Player
import GameData
import Interactions
import Scripts
import Tasks

fortress :: Game ()
fortress = do
  let esra = Person "Esra" "woman6"
      karfrew = Person "Karfrew" "man2"
  void $ createEntity cameraProto
    { ePos      = Just $ V2 512 $ -200
    }

  player <- createEntity playerEntity
    { ePos = Just $ V2 512 (-200)
    , eVel = Just $ V2 0 100
    , eOnDeathScript = Just $ do
        async $ do
          sleep betweenTransmissionsTime
          doConversation
            [ (karfrew, "They came from.... behind.")
            , (esra, "Bummer dude.")
            ]
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

