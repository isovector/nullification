module Level.MainMenu where

import Drawing
import Entity.Camera
import Tasks


mainMenu :: Game ()
mainMenu = do
  void $ createEntity cameraProto
    { ePos = Just 0
    }

  void $ createEntity newEntity
    { ePos = Just $ V2 (-300) 140
    , eGfx = Just $ do
        pure $ move (-280) $ withAlpha 0.5 $ toForm $ image "assets/planet.png"
    }

  void $ createEntity newEntity
    { ePos = Just $ V2 0 0
    , eGfx = Just $ do
        pure $ move (V2 0 (-230)) $ draw_text white CenterAligned "N U L L I F I C A T I O N"
    , eFocused = Just ()
    , eVel = Just $ V2 (-1) 0
    }

  void $ createEntity $ shipProto 80     37  1.4
  void $ createEntity $ shipProto (-100) 100 1.5
  void $ createEntity $ shipProto 200    120 1.1
  void $ createEntity $ shipProto 250    130 1.3
  void $ createEntity $ shipProto 0      150 0.9
  void $ createEntity $ shipProto 50     200 1
  void $ createEntity $ shipProto (-50)  300 0.7



--   player <- createEntity playerProto
--     { ePos = Just $ V2 512 (-200)
--     , eVel = Just $ V2 0 100
--     , eOnDeathScript = Just $ do
--         async $ do
--           command $ Transmit karfrew "They came from.... behind."
--           command $ Transmit esra "BYE BYE BYE"
--     }

--   let mkWall x y =
--         void $ createEntity wall
--           { ePos = Just $ V2 (x * 127) (y * 127)
--           }


--   for_ [-3..2] $ \x -> mkWall x 0

--   traverse_ (uncurry mkWall) $ (,) <$> [-3, 21] <*> [0..10]

--   for_ [5..13] $ \x -> mkWall x 0
--   for_ [16..21] $ \x -> mkWall x 0

--   mkWall (-1) 5
--   mkWall 19 5

--   for_ [2..7] $ \x -> mkWall x 6
--   for_ [11..16] $ \x -> mkWall x 7
--   mkWall 2 7
--   mkWall 2 8
--   mkWall 16 8
--   mkWall 16 9

--   for_ [2..9] $ \x -> mkWall x 9
--   for_ [9..16] $ \x -> mkWall x 10

--   let mkTurret x y =
--         void $ createEntity (turret player)
--           { ePos = Just $ V2 (x * 128) (y * 128)
--           }

--   traverse_ (uncurry mkTurret) $ (,) <$> [1.75, 2.25, 4.75, 5.25] <*> [-1, 1]
--   traverse_ (uncurry mkTurret) $ (,) <$> (fmap (11 +) [1.75, 2.25, 4.75, 5.25]) <*> [-1, 1]

--   let laserFence src dst = newEntity
--         { ePos = Just src
--         , eLaser = Just
--             ( LaserAbsPos dst
--             , \dt -> interact_damage $ 50 * dt
--             )
--         -- , eTeam = Just EnemyTeam
--         }
--       mkLaserFence x1 y1 x2 y2 =
--         void $ createEntity $
--           laserFence (V2 (x1 * 128) (y1 * 128))
--                      (V2 (x2 * 128) (y2 * 128))
--   traverse_ (\(x, y) -> mkLaserFence x y (x + 3) y) $ (,) <$> [2, 13] <*> [-0.1, 0.1]


shipProto :: Double -> Double -> Double -> Entity
shipProto y vel size = newEntity
  { ePos       = Just $ V2 w y
  , eOrigin    = Just $ V2 27 16
  , eDirection = Just $ Radians $ pi
  , eVel       = Just $ V2 (-vel) 0
  , eGfx       = Just $ do
      pure $ withColorMod black $ scale size $ toForm $ image "assets/ship.png"
  , eSpeed     = Just vel
  , eScript    = Just $ forever $ do
      sleep 0.1
      V2 x _ <- query ePos
      case (x <= (-w)) of
        True -> do
          sleep 1.5
          yield $ unchanged
            { ePos = Set $ V2 w y
            }
        False -> yield unchanged
  }
  where w = 600

