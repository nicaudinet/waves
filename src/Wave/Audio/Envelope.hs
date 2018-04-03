{-# LANGUAGE RecordWildCards #-}
module Wave.Audio.Envelope where

import Wave.Audio.Waves

type Volume = Double
data VolumeChange = VC
  { timeTaken :: Time
  , gradient  :: Double
  }

data Envelope = Envelope
  { attack :: VolumeChange
  -- ^ From silence to steady state
  , decay :: VolumeChange
  -- ^ Drop-off between attack peak and steady state
  , sustain :: Time
  -- ^ Time the wave is in the steady state
  , release :: VolumeChange
  -- ^ Fade to silence after steady state
  }

finalVolume :: VolumeChange -> Volume
finalVolume VC{..} = gradient * timeTaken

attackVolume :: Envelope -> Volume
attackVolume Envelope{..} = finalVolume attack

decayVolume :: Envelope -> Volume
decayVolume Envelope{..} =
  finalVolume attack + finalVolume decay

stage :: Envelope -> Time -> Volume
stage env@Envelope{..} t
  | t <  attackTime = (gradient attack) * t
  | t <   decayTime = (gradient decay) * (t - attackTime) + (attackVolume env)
  | t < sustainTime = decayVolume env
  | t < releaseTime = (gradient release) * (t - sustainTime) + (decayVolume env)
  | otherwise       = 0
  where
    attackTime  = timeTaken attack
    decayTime   = attackTime + timeTaken decay
    sustainTime = decayTime  + sustain
    releaseTime = sustainTime + timeTaken release

applyEnvelope :: Envelope -> WaveFn -> WaveFn
applyEnvelope env wave t =
  wave t * (stage env t)
