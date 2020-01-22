module Abilities where

import Actions
import Interactions


------------------------------------------------------------------------------
-- |
ability_blink :: Time -> Double -> Ability
ability_blink blink_time blink_speed =
  defaultAbility
    { abilityPress   = action_blink blink_time blink_speed
    , abilityUnpress = action_blink_unpress
    }

ability_nascentBlink :: Ability
ability_nascentBlink = ability_blink 0.01 5000

ability_shortBlink :: Ability
ability_shortBlink = ability_blink 0.5 400

ability_finalBlink :: Ability
ability_finalBlink = ability_blink 0.5 800


------------------------------------------------------------------------------
-- |
ability_stop :: Ability
ability_stop =
  defaultAbility
    { abilityPress = action_stop
    }


--------------------------------------------------------------------------------
-- |
-- TODO(sandy): this could do rapid fire on abilityDown
ability_shoot :: Time -> Entity -> Ability
ability_shoot lifetime proto =
  defaultAbility
    { abilityPress = action_shoot lifetime proto
    }


ability_laser :: Double -> Double -> Ability
ability_laser dist damage =
  defaultAbility
    { abilityPress = setSelf unchanged
        { eLaser =
            Set
              ( LaserRelPos $ V2 dist 0
              , \dt -> interact_damage $ damage * dt
              )
        }
    , abilityUnpress = setSelf unchanged
        { eLaser = Unset
        }
    }


setSelf :: EntWorld 'SetterOf -> Query ()
setSelf setter = do
  ent <- queryEnt
  command $ Edit ent setter

