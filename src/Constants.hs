module Constants where


gameWidth :: Num a => a
gameWidth = 800

gameHeight :: Num a => a
gameHeight = 600

readingSpeedWordsPerSecond :: Fractional a => a
readingSpeedWordsPerSecond = wpm / 60
  where
    wpm = 300

characterDisplayPerSecond :: Num a => a
characterDisplayPerSecond = 60

minTransmissionTime :: Time
minTransmissionTime = 2

cameraMaximumSnapQd :: Double
cameraMaximumSnapQd = 20 * 20

drawVelocityCutoffQd :: Double
drawVelocityCutoffQd = 80

