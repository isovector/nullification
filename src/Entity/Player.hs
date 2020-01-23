module Entity.Player where

import           Abilities
import qualified Data.Map as M
import           GameData

playerProto :: Entity
playerProto = newEntity
  { eOrigin    = Just $ V2 27 16
  , eDirection = Just $ Radians $ pi / 2
  , eVel       = Just $ V2 0 0
  , eGfx       = Just $ do
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

