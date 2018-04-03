module Main where

import Data.ByteString.Lazy.Builder
import Data.Monoid ((<>))
import System.IO

import Wave.Audio.Pitch
import Wave.Audio.Waves
import Wave.Audio.Envelope
import Wave.Builder
import Wave.Sampling

audio :: WaveFn
audio = applyEnvelope env (applyFreq a4 sineWave)
  where env = Envelope (VC 0.5 2) (VC 0.2 (-2)) 2 (VC 0.3 (-2))

channels = 1
sampleRate = 44100
bitDepth = 16
samples = sampleInt16 audio (1 / 44100.0) 8
waveFile = WaveFile channels sampleRate bitDepth samples

main :: IO ()
main = do
  let filename = "a.wav"
      build = \handle -> hPutBuilder handle (waveFilePcm16Builder waveFile)
  withFile filename WriteMode build
