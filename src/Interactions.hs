module Interactions where

import Geometry
import Types


interact_velToPos :: Time -> Interaction
interact_velToPos dt = do
  pos <- query ePos
  vel <- query eVel
  pure unchanged
    { ePos = Set $ pos + dt *^ vel
    }


interact_accToVel :: Time -> Interaction
interact_accToVel dt = do
  vel <- query eVel
  acc <- query eAcc
  pure unchanged
    { eVel = Set $ vel + dt *^ acc
    }


interact_controlledByPlayer :: V2 -> Interaction
interact_controlledByPlayer dir = do
  with eControlled
  speed <- query eSpeed
  pure unchanged
    { eAcc = Set $ speed *^ dir
    }


interact_rotatingBody :: Time -> Interaction
interact_rotatingBody dt = do
  Radians dir <- query eDirection
  Radians speed <- query eRotationSpeed
  pure unchanged
    { eDirection = Set $ Radians $ dir + speed * dt
    }



data LaserInteraction = LaserInteraction
  { liSrcPos      :: V2
  , liDirection   :: Angle
  , liTeam        :: Team
  , liLaser       :: Laser
  , liInteraction :: Interaction
  }

interact_laserDamage :: [LaserInteraction] -> Interaction
interact_laserDamage lasers = do
  pos   <- query ePos
  hurts <- fmap (moveBox pos) <$> query eHurtboxes
  team  <- queryDef NeutralTeam eTeam

  case find (flip any hurts . laserIntersection) $ filter ((/= team) . liTeam) lasers of
    Just x  -> liInteraction x
    Nothing -> pure unchanged


laserIntersection :: LaserInteraction -> Box -> Bool
laserIntersection li box =
  case liLaser li of
    LaserAbsPos dest ->
      lineIntersectsRect (Line src dest) box
    LaserRelPos rel ->
      lineIntersectsRect (Line src $ src + rotateV2 dir rel) box
  where
    src = liSrcPos li
    dir = liDirection li

