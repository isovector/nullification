module GameData where

import Game.Sequoia.Color
import Actions
import Tasks

gun :: Entity
gun = newEntity
  { eGfx = Just $ do
      pure $ filled red $ rect 2 2
  , eVel = Just $ V2 0 0
  , eSpeed = Just 200
  , eHurtboxes = Just
      [ Rectangle (V2 (-2) (-2)) $ V2 4 4
      ]
  , eHitboxes = Just
      [ ( Rectangle (V2 (-2) (-2)) $ V2 4 4
        , pure delEntity
        )
      ]
  }


wall :: Entity
wall = newEntity
  { eGfx = Just $ pure $ toForm $ image "assets/wall.png"
  , eHitboxes  = Just [(Rectangle (V2 0 0) $ V2 128 128, pure delEntity)]
  }


turret :: Ent -> Entity
turret player = newEntity
  { eGfx = Just $ do
      pure $ filled red $ circle 10
  , eScript = Just $ mconcat
      [ forever $ do
          sleep 0.5
          action_shootAt 2 gun player
      ]
  , eTeam = Just EnemyTeam
  }
