module Actions where

import Game.Sequoia.Color
import Tasks

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

