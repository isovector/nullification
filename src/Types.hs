{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

type Flag  f   = Component f 'Field ()
type Field f a = Component f 'Field a

data EntWorld f = World
  { ePos        :: Field f V2
  , eVel        :: Field f V2
  , eAcc        :: Field f V2
  , eGfx        :: Field f (Query Form)
  , eSpeed      :: Field f Double
  , eDirection  :: Field f Angle

  , eHitboxes   :: Field f [(Box, Interaction)]
  , eHurtboxes  :: Field f [Box]
  , eTeam :: Field f Team

  , eLaser         :: Field f (Laser, Interaction)
  , eRotationSpeed :: Field f Angle

  , eControlled :: Flag f
  } deriving (Generic)

type Entity = EntWorld 'FieldOf


type Query = QueryT EntWorld IO
type Interaction = Query (EntWorld 'SetterOf)
type Game = SystemT EntWorld IO

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

