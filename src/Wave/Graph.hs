module Wave.Graph where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Wave.Audio.Waves

toPair :: WaveFn -> Double -> (Double, Double)
toPair wave t = (t, wave t)

graph :: String -> WaveFn -> IO ()
graph name wave = toFile def (name ++ ".png") $ do
    setColors [opaque blue, opaque red]
    plot (line name [fmap (toPair wave) [0,(0.01)..3]])
