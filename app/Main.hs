module Main where

import Data.ByteString.Lazy.Builder
import Data.Monoid ((<>))
import System.IO

import Wave.Audio.Pitch
import Wave.Audio.Waves
import Wave.Builder
import Wave.Sampling

audio :: WaveFn
audio = applyProgression cgafProg sineWave

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
