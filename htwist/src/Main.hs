{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           HTwist.Utils
import           HTwist.Types
import           HTwist.Listener

import           Control.Monad.Reader
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           System.MIDI
import           System.MIDI.Utility
import           Numeric
import           Foreign.C

changeValue :: (ToCC a) => Connection -> CCNumber -> a -> Env TwisterState ()
changeValue out knb n = liftIO $ send out $ MidiMessage (toCCChannel n) (CC knb (toCCValue n))

changeValue' :: (ToCC a) => (CCValue -> a) -> CCFunction
changeValue' cfn out knb v = changeValue out knb $ cfn v

changeAll ::CCFunction -> CCFunction
changeAll fn con _ val = mapM_ (\n -> fn con n val) [0..15]

cycleThroughColors :: Connection -> CCNumber -> CCValue -> Env TwisterState ()
cycleThroughColors out n vl = changeValue out n cl
    where
        loColor = fromEnum (minBound :: Color)
        hiColor = fromEnum (maxBound :: Color)
        vl'     = hardLerpInts 1 126 loColor hiColor vl
        cl      = toEnum vl' :: Color

selectTwisterInput :: IO Source
selectTwisterInput = selectInputDevice "Midi Fighter Twister" (Just "Midi Fighter Twister")

selectTwisterOutput :: IO Destination
selectTwisterOutput = selectOutputDevice "Midi Fighter Twister" (Just "Midi Fighter Twister")

main :: IO ()
main = do
    let ls  = mempty
        ls' = addCCListeners ls
            $ [ makeCCListener (\me  -> liftIO $ print me) Nothing Nothing
              , makeCCListener (\_   -> liftIO $ print "doing it on channel 1") (Just 1) Nothing
              , makeCCListener (runCCFunction $ changeAll (changeValue' KnobStrobe)) (Just 1) (Just 12)
              , makeCCListener (runCCFunction $ changeAll (changeValue' KnobPulse)) (Just 1) (Just 13)
              , makeCCListener (runCCFunction $ changeAll (changeValue' KnobBrightness)) (Just 1) (Just 15)
              , makeCCListener (runCCFunction $ changeAll (changeValue' IndicatorBrightness)) (Just 1) (Just 14) ]
            <> makeCCListenersFor (runCCFunction cycleThroughColors) (Just 1) [0..11]

    src  <- selectTwisterInput
    dest <- selectTwisterOutput
    out  <- openDestination dest
    let ts = TwisterState src out 0
    tref <- newIORef ts

    inpt <- openSource src (Just $ printCallback tref ls')

    start inpt
    send out (MidiMessage 2 (CC 15 50))
    _ <- getLine
    stop inpt
    ts' <- readIORef tref
    print (tsNum ts')


close :: Connection -> IO ()
close = stop

test :: IO ()
test = main
