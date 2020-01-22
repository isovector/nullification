module Interactions where

import Geometry
import Control.Monad.Coroutine (resume)
import Control.Monad.Coroutine.SuspensionFunctors
import Linear.V2 (angle)
import Linear.Metric (qd)
import Scripts


interact_manageHitpoints :: Interaction
interact_manageHitpoints = do
  void interact_onlyIfOnScreen
  hp <- query eHitpoints
  case hp <= 0 of
    True  -> pure delEntity
    False -> pure unchanged


interact_focusCamera :: Interaction
interact_focusCamera = do
  with eIsCamera
  is_focused <- queryUnique eFocused
  case is_focused of
    Nothing -> do
      pure unchanged
        { eVel = Unset
        }
    Just (focused, _) -> do
      pos <- query ePos
      focus_pos <- focus focused $ query ePos
      pure $ case qd pos focus_pos <= 400 of
        True ->
          unchanged
            { ePos = Set focus_pos
            , eScript = Unset
            , eVel = Unset
            }
        False ->
          unchanged
            { eScript = Set $ script_goTo focus_pos 800 1
            }


interact_age :: Time -> Interaction
interact_age dt = do
  age <- query eAge
  pure unchanged
    { eAge = Set $ age + dt
    }


interact_velToPos :: Time -> Interaction
interact_velToPos dt = do
  pos <- interact_onlyIfOnScreen
  vel <- query eVel
  pure unchanged
    { ePos = Set $ pos + dt *^ vel
    }


interact_accToVel :: Time -> Interaction
interact_accToVel dt = do
  void interact_onlyIfOnScreen
  vel <- query eVel
  acc <- query eAcc
  pure unchanged
    { eVel = Set $ vel + dt *^ acc
    }


interact_controlledByPlayer :: Time -> V2 -> Interaction
interact_controlledByPlayer dt arrs = do
  let rot_speed = 2
  with eControlled
  Radians facing <- query eDirection
  speed  <- query eSpeed

  pure unchanged
    { eAcc = Set $ negate $ speed * view _y arrs *^ angle facing
    , eDirection = Set $ Radians $ facing + rot_speed * dt * view _x arrs
    }


interact_runScript :: Time -> Interaction
interact_runScript dt = do
  script <- query eScript
  resume script >>= \case
    Left (Request change awaited) ->
      pure $ case change == delEntity of
        True -> delEntity
        False ->
          change
            { eScript = Set $ awaited dt
            }
    Right () ->
      pure unchanged
        { eScript = Unset
        }


data LaserInteraction = LaserInteraction
  { liSrcPos      :: V2
  , liDirection   :: Angle
  , liTeam        :: Maybe Team
  , liLaser       :: Laser
  , liInteraction :: Time -> Interaction
  }


interact_hitbox :: [(V2, Maybe Team, Ent, Bool, [(Box, Interaction)])] -> Interaction
interact_hitbox hitboxes = do
  pos   <- interact_onlyIfOnScreen
  hurts <- fmap (moveBox pos) <$> query eHurtboxes
  team  <- queryDef NeutralTeam eTeam
  let normalized_hitboxes = do
        (v2, t, e, m, hs) <- hitboxes
        guard $ t /= Just team
        fmap (uncurry ((,,,) e m) . first (moveBox v2)) hs

  case find (\(b1, (_, _, b2, _)) -> boxIntersectsBox b1 b2)
         . liftA2 (,) hurts
         $ normalized_hitboxes of
    Just (_, (e, m, _, interaction)) -> do
      when m $ command $ Edit e delEntity
      interaction
    Nothing -> pure unchanged



------------------------------------------------------------------------------
-- | this should do DPS, not damage per FRAME
interact_laserDamage :: Time -> [LaserInteraction] -> Interaction
interact_laserDamage dt lasers = do
  pos   <- interact_onlyIfOnScreen
  hurts <- fmap (moveBox pos) <$> query eHurtboxes
  team  <- queryDef NeutralTeam eTeam

  case find (flip any hurts . laserIntersection) $ filter ((/= Just team) . liTeam) lasers of
    Just x  -> liInteraction x dt
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


interact_damage :: Double -> Interaction
interact_damage damage = do
  hp <- query eHitpoints
  pure unchanged
    { eHitpoints = Set $ max 0 $ hp - damage
    }


interact_onlyIfOnScreen :: Query V2
interact_onlyIfOnScreen = do
  pos <- query ePos
  Just camera_pos <-
    fmap listToMaybe
      . subquery (uniqueEnt eIsCamera)
      $ query ePos
  guard $ qd pos camera_pos <= 800 * 800
  pure pos


