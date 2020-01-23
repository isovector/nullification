{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Types
  ( module Types
  , Music
  , Chunk
  ) where

import           BasePrelude
import           Control.Monad.Coroutine
import           Control.Monad.Coroutine.SuspensionFunctors
import qualified Control.Monad.Fail as MF
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Writer.CPS
import           Data.Binary (Binary)
import           Data.Ecstasy
import qualified Data.Map.Strict as M
import           Game.Sequoia
import           Game.Sequoia.Keyboard (Key)
import           SDL.Mixer (Music, Chunk)

type Flag  f   = Component f 'Field ()
type Field f a = Component f 'Field a

data EntWorld f = World
  { ePos           :: Field f V2
  , eVel           :: Field f V2
  , eAcc           :: Field f V2
  , eSpeed         :: Field f Double
  -- TODO(sandy): facing
  , eDirection     :: Field f Angle

  , eHitpoints     :: Field f Double

  , eOrigin        :: Field f V2
  , eGfx           :: Field f (Query Form)

  , ePlaySfx       :: Field f (SoundBank -> Chunk)

  , eHitboxes      :: Field f [(Box, Interaction)]
  , eHurtboxes     :: Field f [Box]
  , eTeam          :: Field f Team
  , eDieOnContact  :: Flag f

  , eAlive         :: Flag f
  , eAge           :: Field f Time  -- time alive
  , eScript        :: Field f (Task ())
  , eSpecialThing  :: Field f SpecialThing
  , eLifetime      :: Field f Time  -- time to live

  , eDeathState    :: Field f DeathState
  , eOnDeathScript  :: Field f (Task ())

  , eLaser         :: Field f (Laser, Time -> Interaction)

  , eOnMinimap     :: Field f (Color, Double)

  , eControlled    :: Flag f
  , eAbilities     :: Field f Controller
  , eFocused       :: Component f 'Unique ()
  , eIsCamera      :: Component f 'Unique ()
  } deriving (Generic)

instance Eq (SoundBank -> Chunk) where
  _ == _ = False
instance Eq (Time -> Interaction) where
  _ == _ = False
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

instance MonadFail Task where
  fail = lift . fail

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
  | Transmission Person String
  deriving (Eq, Ord, Show)

data Person = Person
  { personName     :: String
  , personPortrait :: String
  }
  deriving (Eq, Ord, Show)


data Keystate = Press | Down | Up | Unpress
  deriving (Eq, Ord, Show)


data SoundBank = SoundBank
  { sfxShot       :: Chunk
  , sfxBlinkStart :: Chunk
  , sfxBlinkEnd   :: Chunk
  , sfxPowerup    :: Chunk
  , sfxMultishot  :: Chunk
  }


data Control
  = Weapon1
  | Weapon2
  | Weapon3
  | Shield
  | Boost
  | Stop
  deriving (Eq, Ord, Show)

type Controller = M.Map Control Ability
type ControlMapping = M.Map Control Key

data Ability = Ability
  { abilityPress   :: Query ()
  , abilityDown    :: Query ()
  , abilityUnpress :: Query ()
  , abilityUp      :: Query ()
  }
  deriving (Eq)

defaultAbility :: Ability
defaultAbility =
  Ability
    (pure ())
    (pure ())
    (pure ())
    (pure ())


data DeathState
  = MarkedForDeath
  | WaitingOnDeathScript
  deriving (Eq, Ord, Show)


data FramePlaybackInfo = FramePlaybackInfo
  { fpiDeltaTime :: !Time
  , fpiDeltaKeys :: !(M.Map Key Bool)
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass Binary

