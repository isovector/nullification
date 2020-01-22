{-# LANGUAGE RecordWildCards #-}

module Assets where

import SDL.Mixer

soundBank :: SoundBank
soundBank = unsafePerformIO $ do
  sfxShot <- load "assets/sounds/Shot.wav"
  pure SoundBank {..}

{-# NOINLINE soundBank #-}

