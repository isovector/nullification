module Actions where

import Tasks
import Scripts


action_blink :: (CanRunCommands m, CanRunQueries m) => m ()
action_blink = do
  parent <- queryEnt
  parent_pos <- query ePos
  parent_vel <- query eVel
  parent_gfx <- query eGfx
  command $ Spawn newEntity
    { ePos = Just parent_pos
    , eVel = Just $ parent_vel * 2
    , eGfx = Just $ do
        gfx <- parent_gfx
        pure $ withAlpha 0.2 $ gfx
    , eScript = Just $ do
        sleep 2
        pos' <- query ePos
        command $ Edit parent unchanged
          { ePos = Set pos'
          }
        yield delEntity
    }


action_shootAt
    :: (CanRunCommands m, CanRunQueries m)
    => Entity
    -> Ent
    -> m ()
action_shootAt proto target = do
  let speed = fromMaybe 100 $ eSpeed proto
  target_pos <- focus target $ query ePos
  parent_pos <- query ePos

  command $ Spawn proto
    { ePos = Just parent_pos
    , eScript = mconcat
        [ eScript proto
        , Just $ do
            script_goTo target_pos speed 5
            yield delEntity
        ]
    }

