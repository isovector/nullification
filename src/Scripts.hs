module Scripts where

import Constants
import Tasks

script_rotate :: Angle -> Time -> Task ()
script_rotate (Radians change) duration = do
  Radians angle <- query eDirection
  over duration $ \(Progress p) -> do
    void $ request unchanged
      { eDirection = Set $ Radians $ angle + change * p
      }


script_goTowards :: V2 -> Double -> Task ()
script_goTowards dst speed = do
  src <- query ePos
  let vel = normalize (dst - src) ^* speed
  yield unchanged
    { eVel = Set vel
    }

script_goTo :: V2 -> Double -> Double -> Task ()
script_goTo dst speed radius = do
  script_goTowards dst speed
  fix $ \loop -> do
    sleep 0.02
    pos <- query ePos
    case norm (pos - dst) <= radius of
      True -> yield unchanged
        { eVel = Set 0
        }
      False -> loop


script_die :: Task ()
script_die = yield delEntity


doTransmission :: CanRunCommands m => Person -> String -> m Time
doTransmission person msg = do
  let wordcount = fromIntegral $ length $ words msg
      charcount = fromIntegral $ length msg
      wanted_time = wordcount / readingSpeedWordsPerSecond
                  + charcount / characterDisplayPerSecond
      time = max minTransmissionTime wanted_time
  command $ Spawn newEntity
    { eSpecialThing = Just $ Transmission person msg
    , eLifetime     = Just time
    , eAge          = Just 0
    }
  pure time


doConversation :: [(Person, String)] -> Task ()
doConversation [] = pure ()
doConversation ((person, msg) : convo) = do
  time <- doTransmission person msg
  sleep $ time + betweenTransmissionsTime
  doConversation convo

