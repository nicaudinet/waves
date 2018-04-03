module Wave.Audio.Pitch where

import Wave.Audio.Waves
import Data.List

import Debug.Trace

applyFreq :: Double -> WaveFn -> WaveFn
applyFreq pitch w = \t -> w (t * pitch)

a4 :: Double
a4 = 440

c4 :: Double
c4 = intervalRatio 3 . octaveDw $ a4

e4 :: Double
e4 = intervalRatio 4 c4

octaveUp = (* 2)
octaveDw = (/ 2)

type Interval = Double -> WaveFn -> WaveFn

intervalRatio :: Int -> Double -> Double
intervalRatio c r = r * (2 ** (fromIntegral c / 12))

interval :: Int -> Interval
interval c r w
  | c <= 0    = applyFreq r w
  | otherwise = applyFreq (intervalRatio c r) w

-- Common intervals
root     = interval 0
semitone = interval 1
tone     = interval 2
minor3   = interval 3
major3   = interval 4
fourth   = interval 5
dim5     = interval 6
fifth    = interval 7
aug5     = interval 8
sixth    = interval 9
minor7   = interval 10
major7   = interval 11

type Chord = [Interval]

applyChord :: Double -> WaveFn -> Chord -> WaveFn
applyChord pitch wave chord = sumWaves $ chord <*> pure pitch <*> pure wave

buildChord :: [Int] -> Chord
buildChord = fmap interval

majorChord :: Chord
majorChord = buildChord [0, 4, 7]

minorChord :: Chord
minorChord = buildChord [0, 3, 7]

major7Chord :: Chord
major7Chord = buildChord [0, 4, 7, 11]

type Duration = Int
data Progression =
  Progression
    { chords :: [((Chord, Int), Duration)]
    , key    :: Double
    , tempo  :: Double
    }

applyProgression :: Progression -> WaveFn -> WaveFn
applyProgression prog wave t
  | t == 0    = (applyChord pitch' wave chord') t
  | t > last durations = 0
  | otherwise = (applyChord pitch  wave chord ) t
  where
    (chord', ival') = fst (head (chords prog))
    pitch' = intervalRatio ival' (key prog)
    fn = \(a, b) -> fromIntegral b / tempo prog
    durations = scanl (+) 0 (fmap fn (chords prog))
    Just index = pred <$> findIndex (> t) durations
    ( chord,  ival) = fst ((chords prog) !! index)
    pitch = intervalRatio ival (key prog)

-- Nice chord progression with the C major 7 to the E minor
ccegProg :: Progression
ccegProg = Progression chords c4 2
  where chords =
          [ (( majorChord, 0), 2)
          , ((major7Chord, 0), 2)
          , (( minorChord, 4), 2)
          , (( majorChord, 7), 2)
          ]

-- Typical pop 4-chord progression
cgafProg :: Progression
cgafProg = Progression chords c4 4
  where chords =
          [ ((majorChord, 0), 2)
          , ((majorChord, 7), 2)
          , ((minorChord, 9), 2)
          , ((majorChord, 5), 2)
          ]

