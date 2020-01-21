module Actions where

import Tasks
import Scripts
import Geometry


action_blink :: (CanRunCommands m, CanRunQueries m) => m ()
action_blink = do
  let blink_speed = 100
      blink_time  = 0.5

  parent <- queryEnt
  parent_pos <- query ePos
  parent_dir <- query eDirection
  parent_vel <- query eVel
  parent_gfx <- query eGfx
  command $ Spawn newEntity
    { ePos = Just parent_pos
    , eVel = Just $ parent_vel + rotateV2 parent_dir (V2 blink_speed 0)
    , eDirection = Just $ parent_dir
    , eGfx = Just $ do
        gfx <- parent_gfx
        pure $ withAlpha 0.4 $ gfx
    , eScript = Just $ do
        sleep blink_time
        pos' <- query ePos
        command $ Edit parent unchanged
          { ePos = Set pos'
          }
        yield delEntity
    , eSpecialThing = Just $ BlinkFor parent
    }


action_stop :: (CanRunCommands m, CanRunQueries m) => m ()
action_stop = do
  ent <- queryEnt
  command $ Edit ent unchanged
    { eVel = Set 0
    }



action_shootAt
    :: (CanRunCommands m, CanRunQueries m)
    => Time
    -> Entity
    -> Ent
    -> m ()
action_shootAt lifetime proto target = do
  let speed = fromMaybe 100 $ eSpeed proto
  target_pos <- focus target $ query ePos
  parent_pos <- query ePos
  parent_team <- queryMaybe eTeam

  command $ Spawn proto
    { ePos = Just parent_pos
    , eTeam = parent_team
    , eScript = mconcat
        [ eScript proto
        , Just $ script_goTowards target_pos speed
        , Just $ do
            sleep lifetime
            script_die
        ]
    }

