{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Sound.RtMidi
import           Numeric
import           Foreign.C

callback :: CDouble -> [CUChar] -> IO ()
callback dlt msg = putStrLn $ (foldr (showHex. fromEnum) "" msg) ++ " - " ++ (show dlt)

test :: IO ()
test = do
    i <- defaultInput
    openPort i 0 "RtMidi"
    setCallback i callback
    _ <- getLine
    return ()

main :: IO ()
main = putStrLn "Hello, Haskell!"
