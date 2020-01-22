module GameData where

import Game.Sequoia.Color
import Actions
import Tasks
import Interactions

gun :: Time -> Entity
gun lifetime = newEntity
  { eGfx = Just $ do
      pure $ filled red $ rect 2 2
  , eVel = Just $ V2 0 0
  , eSpeed = Just 200
  , eLifetime = Just lifetime
  , eHitboxes = Just
      [ ( Rectangle (V2 (-2) (-2)) $ V2 4 4
        , interact_damage 1
        )
      ]
  , eDieOnContact = Just ()
  }


clusterGun :: Time -> Int -> Entity -> Entity
clusterGun lifetime num child = newEntity
  { eGfx = Just $ do
      pure $ filled purple $ circle 3
  , eVel = Just $ V2 0 0
  , eSpeed = Just 200
  , eHurtboxes = Just
      [ Rectangle (-1.5) 3
      ]
  , eScript = Just $ do
      sleep lifetime
      action_multishot (Radians $ 2 * pi) num child
      me <- queryEnt
      command $ Edit me delEntity
  }


wall :: Entity
wall = newEntity
  { eGfx = Just $ pure $ toForm $ image "assets/wall.png"
  , eOrigin = Just 64
  , eHitboxes  = Just [(Rectangle (-64) 126, pure delEntity)]
  , eOnMinimap = Just (grey, 4)
  }


turret :: Ent -> Entity
turret player = newEntity
  { eGfx = Just $ do
      pure $ filled red $ circle 10
  , eScript = Just $ mconcat
      [ forever $ do
          sleep 0.5
          action_shootAt (gun 2) player
      ]
  , eHitpoints = Just 1
  , eHurtboxes = Just [Rectangle (-10) 20]
  , eTeam = Just EnemyTeam
  , eOnMinimap = Just (red, 1.5)
  }


collectable :: Interaction -> Entity
collectable interaction = newEntity
  { eGfx = Just $ pure $ filled green $ rect 10 10
  , eHitboxes = Just
      [ ( Rectangle (-5) 10
        , do
            command $ Sfx sfxPowerup
            interaction
        )
      ]
  , eDieOnContact = Just ()
  }

