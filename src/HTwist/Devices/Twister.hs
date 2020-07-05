{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HTwist.Devices.Twister where

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

-- Example functions for controlling the values, colors, and brightness for the
-- MIDI Fighter Twister:

changeValue :: (ToCC a) => Connection -> CCNumber -> a -> Env TwisterState ()
changeValue out knb n = liftIO $ send out $ mkCCMessage (toCCChannel n) knb (toCCValue n)

changeValue' :: (ToCC a) => (CCValue -> a) -> CCFunction
changeValue' cfn out knb v = changeValue out knb $ cfn v

changeAll :: CCFunction -> CCFunction
changeAll fn con _ val = mapM_ (\n -> fn con n val) [0..15]

cycleThroughColors :: Connection -> CCNumber -> CCValue -> Env TwisterState ()
cycleThroughColors out n (CCValue vl) = changeValue out n cl
    where
        loColor = fromEnum (minBound :: Color)
        hiColor = fromEnum (maxBound :: Color)
        vl'     = hardLerpInts 1 126 loColor hiColor vl
        cl      = toEnum vl' :: Color

selectTwisterInput :: IO Source
selectTwisterInput = selectInputDevice "Midi Fighter Twister" (Just "Midi Fighter Twister")

selectTwisterOutput :: IO Destination
selectTwisterOutput = selectOutputDevice "Midi Fighter Twister" (Just "Midi Fighter Twister")
