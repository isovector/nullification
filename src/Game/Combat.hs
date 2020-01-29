module Game.Combat where

import Interactions


runLasers :: Time -> Game ()
runLasers dt = do
  lasers <- efor (entsWith eLaser) $ do
    interact_onlyIfAlive
    pos  <- interact_posIfOnScreen
    team <- queryMaybe eTeam
    dir  <- queryDef 0 eDirection
    (laser, action) <- query eLaser
    pure $ LaserInteraction pos dir team laser action
  emap (entsWith eHurtboxes) $ interact_laserDamage dt lasers


runHitboxes :: Game ()
runHitboxes = do
  hitboxes <- efor (entsWith eHitboxes) $ do
    interact_onlyIfAlive
    (,,,,)
      <$> interact_posIfOnScreen
      <*> queryMaybe eTeam
      <*> queryEnt
      <*> queryFlag eDieOnContact
      <*> query eHitboxes
  emap (entsWith eHurtboxes) $ interact_hitbox hitboxes

