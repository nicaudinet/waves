{-# LANGUAGE FlexibleInstances #-}

module Wave.Audio.Waves where

import Data.Monoid

type WaveFn = Double -> Double

instance Monoid Double where
  mempty  = 0
  mappend = (+)

combine
  :: (a -> a -> a)
  -> (a -> a)
  -> (a -> a)
  -> (a -> a)
combine comb f g = \t -> comb (f t) (g t)

instance Num WaveFn where
  f + g = combine (+) f g
  f - g = combine (-) f g
  f * g = combine (*) f g

-- What one would really want is a  Nat typeclass
-- of numbers that don't have a sign.
  abs = id
  signum _ = const 1
  fromInteger x = scaleA (fromInteger x) (const 1)

constWave :: Double -> WaveFn
constWave c = const c

scaleA :: Double -> WaveFn -> WaveFn
scaleA c f = (* c) . f

sumWaves :: [WaveFn] -> WaveFn
sumWaves ws = scaleA c (mconcat ws)
  where c = recip $ fromIntegral (length ws)

-- Different types of basic waves
-- t is the time step, and is assumed to start at 0

periodic :: Double -> Double
periodic t = t - fromInteger (floor t)

periodically :: WaveFn -> WaveFn
periodically f = f . periodic

sineWave :: WaveFn
sineWave t = sin (t * 2 * pi)

squareWave :: WaveFn
squareWave t =
  if sineWave t > 0 then 1 else -1

saw :: WaveFn
saw t = 2 * t - 1

sawWave :: WaveFn
sawWave = periodically saw

triangle :: WaveFn
triangle t
  | t <= 0.5  =    4 * t - 1
  | otherwise = (-4) * t + 3

triangleWave :: WaveFn
triangleWave = periodically triangle
