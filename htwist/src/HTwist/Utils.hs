module HTwist.Utils where

lerp :: Fractional a => a -> a -> a -> a -> a -> a
lerp inLo inHi outLo outHi v
    = outLo + (outHi - outLo) * ((v - inLo) / (inHi - inLo))

-- |Linear interpolation from range inLo inHi to outLo outHi, given a value
-- between inLo and inHi, which then ensures it is in the output Range
hardLerp :: (Fractional a, Ord a, Show a) => a -> a -> a -> a -> a -> a
hardLerp inLo inHi outLo outHi v = (max outLo . min outHi) lerped
    where
        lerped = lerp inLo inHi outLo outHi v

-- hardLerpInts :: Int -> Int -> Int -> Int -> Int -> Int
hardLerpInts :: (Num a, Integral a) => a -> a -> a -> a -> a -> a
hardLerpInts inLo inHi outLo outHi v
    = round $ hardLerp (fromIntegral inLo)
                       (fromIntegral inHi)
                       (fromIntegral outLo)
                       (fromIntegral outHi)
                       (fromIntegral v)

lerpInts :: Int -> Int -> Int -> Int -> Int -> Int
lerpInts inLo inHi outLo outHi v
    = round $ lerp (fromIntegral inLo)
                   (fromIntegral inHi)
                   (fromIntegral outLo)
                   (fromIntegral outHi)
                   (fromIntegral v)

