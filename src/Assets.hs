{-# LANGUAGE RecordWildCards #-}

module Assets where

import SDL.Mixer

soundBank :: SoundBank
soundBank = unsafePerformIO $ do
  sfxShot       <- load "assets/sounds/laser1.ogg"
  sfxBlinkStart <- load "assets/sounds/enchant.ogg"
  sfxBlinkEnd   <- load "assets/sounds/warp3.ogg"
  sfxPowerup    <- load "assets/sounds/powerUp3.ogg"
  pure SoundBank {..}

{-# NOINLINE soundBank #-}

