module Wave.Sampling where

import Data.Int

sampleTimes :: Double -> Double -> Double -> [Double]
sampleTimes period start end
  | start > end = []
  | otherwise = start : sampleTimes period (start + period) end

sample :: (Double -> Double) -> Double -> Double -> [Double]
sample audio period duration = fmap audio (sampleTimes period 0 duration)

sampleInt16 :: (Double -> Double) -> Double -> Double -> [Int16]
sampleInt16 audio period duration =
  let samples = sample audio period duration
  in fmap (\x -> floor (x * 32767.5)) samples
