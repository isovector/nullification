module Actions where

import Tasks
import Scripts
import Geometry


-- Runs for the using entity
action_blink :: (CanRunCommands m, CanRunQueries m) => Time -> Double ->  m ()
action_blink blink_time blink_speed = do
  parent <- queryEnt
  other_blinks <- subquery (entsWith eSpecialThing) $ do
    BlinkFor ent <- query eSpecialThing
    guard $ parent == ent

  when (null other_blinks) $ do
    parent_pos <- query ePos
    parent_dir <- query eDirection
    parent_vel <- query eVel
    parent_gfx <- query eGfx
    parent_origin <- queryMaybe eOrigin
    command $ Sfx sfxBlinkStart
    command $ Spawn newEntity
      { ePos = Just parent_pos
      , eVel = Just $ parent_vel + rotateV2 parent_dir (V2 blink_speed 0)
      , eDirection = Just $ parent_dir
      , eGfx = Just $ do
          gfx <- parent_gfx
          pure $ withAlpha 0.4 $ gfx
      , eOrigin = parent_origin
      , eScript = Just $ do
          sleep blink_time
          action_blink_finish
      , eSpecialThing = Just $ BlinkFor parent
      }


-- Runs for the using entity
-- TODO(sandy): This is disgusting, and does some trickery to switch from the
-- player's query context into the blink entity's
-- TODO(sandy): maybe we could inline this into the blink ability
action_blink_unpress :: (CanRunCommands m, CanRunQueries m) => m ()
action_blink_unpress = do
  parent <- queryEnt
  -- Find the entity which is the BlinkFor this entity
  blinks <- subquery (entsWith eSpecialThing) $ do
    ent <- queryEnt
    BlinkFor ent_parent <- query eSpecialThing
    guard $ parent == ent_parent
    pure ent
  void $ subquery (someEnts blinks) action_blink_finish



-- Runs for the blink entity
action_blink_finish :: (CanRunCommands m, CanRunQueries m) => m ()
action_blink_finish = do
  ent <- queryEnt
  BlinkFor parent <- query eSpecialThing
  pos' <- query ePos
  commands
    [ Sfx sfxBlinkEnd
    , Edit parent unchanged
        { ePos = Set pos'
        }
    , Edit ent delEntity
    ]




action_stop :: (CanRunCommands m, CanRunQueries m) => m ()
action_stop = do
  ent <- queryEnt
  command $ Edit ent unchanged
    { eVel = Set 0
    }


action_shootAt
    :: (CanRunCommands m, CanRunQueries m)
    => Entity
    -> Ent
    -> m ()
action_shootAt proto target = do
  let speed = fromMaybe 100 $ eSpeed proto
      lifetime = fromMaybe 100 $ eLifetime proto
  target_pos <- focus target $ query ePos
  parent_pos <- query ePos


  -- Only shoot if you're in range
  case (quadrance (target_pos - parent_pos) <= (speed * lifetime) * (speed * lifetime)) of
    False -> pure ()
    True -> do
      parent_team <- queryMaybe eTeam

      command $ Spawn proto
        { ePos      = Just parent_pos
        , eTeam     = parent_team
        , eScript   = mconcat
            [ eScript proto
            , Just $ script_goTowards target_pos speed
            ]
        }


action_shootDir
    :: (CanRunCommands m, CanRunQueries m)
    => Angle
    -> Entity
    -> m ()
action_shootDir dir proto = do
  let speed = fromMaybe 100 $ eSpeed proto
  parent_pos  <- query ePos
  parent_vel  <- queryDef 0 eVel
  parent_team <- queryMaybe eTeam

  command $ Spawn proto
    { ePos       = Just parent_pos
    , eVel       = Just $ parent_vel + rotateV2 dir (V2 speed 0)
    , eDirection = Just dir
    , eTeam      = parent_team
    }

action_shoot
    :: (CanRunCommands m, CanRunQueries m)
    => Entity
    -> m ()
action_shoot proto = do
  dir  <- query eDirection
  action_shootDir dir proto


action_multishot
    :: (CanRunCommands m, CanRunQueries m)
    => Angle
    -> Int
    -> Entity
    -> m ()
action_multishot _ 1 proto = action_shoot proto
action_multishot (Radians spread) num proto = do
  Radians parent_dir <- query eDirection
  let start_dir = parent_dir - spread / 2

  for_ [0 .. num - 1] $ \n -> do
    action_shootDir
      (Radians $ start_dir + (fromIntegral n + 0.5) * spread / fromIntegral num)
      $ case n of
          0 -> proto
          _ -> proto { ePlaySfx = Nothing }

