{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           HTwist.Utils
import           HTwist.Types
import           HTwist.Listener

import           HTwist.Devices.Twister

import           Control.Monad.Reader
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           System.MIDI
import           System.MIDI.Utility
import           Numeric
import           Foreign.C

-- Example of use with the MIDI Fighter Twister. The functions for controlling the Twister are found
-- in HTwist.Devices.Twister, however the guts of the listener funcitonality is in HTwist.Listener.
main :: IO ()
main = do
    let ls = addCCListeners mempty                                                         -- channel  cc number
            $ [ makeCCListener (\me  -> liftIO $ print $ "Midi event: " <> show me)           Nothing  Nothing
              , makeCCListener (\_   -> liftIO $ print "All knobs on channel 1")              (Just 1) Nothing
              , makeCCListener (runCCFunction $ changeAll (changeValue' KnobStrobe))          (Just 1) (Just 12)
              , makeCCListener (runCCFunction $ changeAll (changeValue' KnobPulse))           (Just 1) (Just 13)
              , makeCCListener (runCCFunction $ changeAll (changeValue' KnobBrightness))      (Just 1) (Just 15)
              , makeCCListener (runCCFunction $ changeAll (changeValue' IndicatorBrightness)) (Just 1) (Just 14) ]
            <> makeCCListenersFor (runCCFunction cycleThroughColors) (Just 1) [0..11]

    src  <- selectTwisterInput
    dest <- selectTwisterOutput
    out  <- openDestination dest

    let ts = TwisterState src out mempty 0
    tref <- newIORef ts

    inpt <- openSource src (Just $ listenerCallback tref ls)
    start inpt

    _ <- getLine

    stop inpt


close :: Connection -> IO ()
close = stop

test :: IO ()
test = main
