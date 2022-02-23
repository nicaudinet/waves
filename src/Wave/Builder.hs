module Wave.Builder where

import Data.ByteString.Builder
import Data.Int
import Data.Word

-- channels, sample rate, bit depth, samples
data WaveFile a = WaveFile Int Int Int [a]

-- Resource Interchange File Format
data RiffFile = RiffFile [RiffChunk]
data RiffHeader = RiffHeader String Int
data RiffChunk =
    RiffChunk RiffHeader [Word8]
  | RiffFormChunk String [RiffChunk]
  | WaveFormatChunk Int Int Int Int [Word8] -- format, channels, sample rate, bit depth, extra
  | WaveInt16SamplesChunk [Int16]

pcmFormat = 1

-- Pulse Code Modulation
riffFileForWavePcm16 :: WaveFile Int16 -> RiffFile
riffFileForWavePcm16 (WaveFile channels sampleRate bitDepth samples) =
  RiffFile
    [ RiffFormChunk "WAVE"
      [ WaveFormatChunk pcmFormat channels sampleRate bitDepth []
      , WaveInt16SamplesChunk samples
      ]
    ]

waveFilePcm16Builder :: WaveFile Int16 -> Builder
waveFilePcm16Builder waveFile = riffFileBuilder $ riffFileForWavePcm16  waveFile

riffFileBuilder :: RiffFile -> Builder
riffFileBuilder (RiffFile chunks) = mconcat $ map riffChunkBuilder chunks

riffChunkBuilder :: RiffChunk -> Builder
riffChunkBuilder (RiffChunk header bytes) =
  mconcat (riffHeaderBuilder header : map word8 bytes)
riffChunkBuilder (RiffFormChunk form chunks) =
  mconcat (
    [ riffHeaderBuilder (RiffHeader "RIFF" (4 + (sizeRiffChunks chunks)))
    , string8 "WAVE"
    ] ++ (map riffChunkBuilder chunks)
    )
riffChunkBuilder (WaveFormatChunk format channels sampleRate bitDepth extraBytes) =
  mconcat (
    [ riffHeaderBuilder $ RiffHeader "fmt " (fromIntegral $ 18 + (length extraBytes))
    ,    word16LE $ fromIntegral format
    ,    word16LE $ fromIntegral channels
    ,    word32LE $ fromIntegral sampleRate
    ,    word32LE $ fromIntegral $ sampleRate * channelsSampleSize
    ,    word16LE $ fromIntegral channelsSampleSize
    ,    word16LE $ fromIntegral bitDepth
    ,    word16LE $ fromIntegral $ length extraBytes
    ] ++ (map word8 extraBytes)
    )
  where channelsSampleSize = channels * (bitDepth `div` 8)
riffChunkBuilder (WaveInt16SamplesChunk samples) =
  mconcat (
    riffHeaderBuilder (RiffHeader "data" (2 * (fromIntegral $ length samples)))
    : (map int16LE samples)
    )


riffHeaderBuilder :: RiffHeader -> Builder
riffHeaderBuilder (RiffHeader tag size) =
  mconcat
    [ string8 tag
    , word32LE $ fromIntegral size
    ]

sizeRiffChunks :: [RiffChunk] -> Int
sizeRiffChunks = sum . map sizeRiffChunk

sizeRiffChunk :: RiffChunk -> Int
sizeRiffChunk chunk = sizeRiffHeader + (sizeRiffContent chunk)

sizeRiffHeader :: Int
sizeRiffHeader = 8

sizeRiffContent :: RiffChunk -> Int
sizeRiffContent (RiffChunk _ bytes) = length bytes
sizeRiffContent (RiffFormChunk _ chunks) = 4 + (sizeRiffChunks chunks)
sizeRiffContent (WaveFormatChunk _ _ _ _ extra) = 18 + (length extra)
sizeRiffContent (WaveInt16SamplesChunk samples) = 2 * (length samples)

