module GameData where

import Game.Sequoia.Color

gun :: Entity
gun = newEntity
  { eGfx = Just $ do
      pos <- query ePos
      pure $ move pos $ filled red $ rect 2 2
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
