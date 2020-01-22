{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Types
  ( module Types
  , Music
  , Chunk
  ) where

import BasePrelude
import Game.Sequoia
import Data.Ecstasy
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import SDL.Mixer (Music, Chunk)

type Flag  f   = Component f 'Field ()
type Field f a = Component f 'Field a

data EntWorld f = World
  { ePos           :: Field f V2
  , eVel           :: Field f V2
  , eAcc           :: Field f V2
  , eSpeed         :: Field f Double
  -- TODO(sandy): facing
  , eDirection     :: Field f Angle

  , eHitpoints     :: Field f Int

  , eOrigin        :: Field f V2
  , eGfx           :: Field f (Query Form)

  , eHitboxes      :: Field f [(Box, Interaction)]
  , eHurtboxes     :: Field f [Box]
  , eTeam          :: Field f Team
  , eDieOnContact  :: Flag f

  , eAlive         :: Flag f
  , eAge           :: Field f Time
  , eScript        :: Field f (Task ())
  , eSpecialThing  :: Field f SpecialThing

  , eLaser         :: Field f (Laser, Interaction)

  , eOnMinimap     :: Field f (Color, Double)

  , eFocused       :: Component f 'Unique ()
  , eIsCamera      :: Component f 'Unique ()
  , eControlled    :: Flag f
  } deriving (Generic)

instance Eq (QueryT _1 _2 _3) where
  _ == _ = False
deriving instance Eq (EntWorld 'SetterOf)
deriving instance Eq (EntWorld 'FieldOf)

instance Eq (Coroutine _1 _2 _3) where
  _ == _ = False

instance Eq a => Eq (Update a) where
  Keep     == Keep  = True
  Keep     == _     = False
  Unset    == Unset = True
  Unset    == _     = False
  Set a    == Set b = a == b
  Set _    == _     = False
  Modify _ == _     = False

type Entity = EntWorld 'FieldOf


type UnderlyingMonad = WriterT [Command] IO

type Query = QueryT EntWorld UnderlyingMonad
type Interaction = Query (EntWorld 'SetterOf)
type Game = SystemT EntWorld UnderlyingMonad

type Task = Coroutine (Request (EntWorld 'SetterOf) Time) Query

data Team
  = NeutralTeam
  | PlayerTeam
  | EnemyTeam
  deriving (Eq, Ord, Show)

data Laser
  = LaserAbsPos V2
  | LaserRelPos V2
  deriving (Eq, Ord, Show)


data Box = Rectangle V2 V2
  deriving (Eq, Ord, Show)

data Line = Line V2 V2
  deriving (Eq, Ord, Show)


newtype Angle = Radians Double
  deriving stock (Eq, Ord, Show)
  deriving newtype Num

data Command
  = Spawn Entity
  | Edit Ent (EntWorld 'SetterOf)
  | Sfx (SoundBank -> Chunk)

newtype Progress = Progress Double
  deriving (Eq, Ord, Show)

data SpecialThing
  = BlinkFor Ent
  deriving (Eq, Ord, Show)


data Keystate = Press | Down | Up | Unpress
  deriving (Eq, Ord, Show)


data SoundBank = SoundBank
  { sfxShot       :: Chunk
  , sfxBlinkStart :: Chunk
  , sfxBlinkEnd   :: Chunk
  , sfxPowerup    :: Chunk
  }

