{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           HTwist.Utils
import           HTwist.Types

-- import           Sound.RtMidi
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           System.MIDI
import           System.MIDI.Utility
import           Numeric
import           Foreign.C

type Env a = ReaderT (IORef a) IO ()

type ListenerFunction = (TwisterState -> MidiEvent -> Env TwisterState) -- IO ())
type CCFunction = (Connection -> CCNumber -> CCValue -> IO ())

type MIDIListener = MidiEvent -> IO ()

data CCListener = CCListener {
         func  :: ListenerFunction
        ,chan  :: Maybe Channel
        ,ccNum :: Maybe CCNumber
    }

-- data TwisterState = TwisterState {
--         num :: Int
--     } deriving (Show,Eq)

makeCCListener
    :: ListenerFunction
    -> Maybe Channel
    -> Maybe CCNumber
    -> CCListener
makeCCListener = CCListener

addCCListeners :: Listeners -> [CCListener] -> Listeners
addCCListeners ls []     = ls
addCCListeners ls (x:xs) = addCCListeners (addCCListener ls x) xs

addCCListener :: Listeners -> CCListener -> Listeners
addCCListener ls (CCListener fn ch nm) = HM.insert ch nmap' ls
    where
        nmap  = fromMaybe mempty $ HM.lookup ch ls
        nmap' = HM.insertWith (<>) nm [fn] nmap

makeCCListenersFor :: ListenerFunction -> Maybe Channel ->  [CCNumber] -> [CCListener]
makeCCListenersFor fn chn = map (\n -> makeCCListener fn chn (Just n))

type Listeners = HM.HashMap (Maybe Channel) (HM.HashMap (Maybe CCNumber) [ListenerFunction])

getListenersFor :: Listeners -> Channel -> CCNumber -> [ListenerFunction]
getListenersFor ls ch nm = nn <> nj <> jn <> jj
    where
        nn = fromMaybe [] $ HM.lookup Nothing ls    >>= HM.lookup Nothing
        nj = fromMaybe [] $ HM.lookup Nothing ls    >>= HM.lookup (Just nm)
        jn = fromMaybe [] $ HM.lookup (Just ch) ls  >>= HM.lookup Nothing
        jj = fromMaybe [] $ HM.lookup (Just ch) ls  >>= HM.lookup (Just nm)

runCCFunction :: CCFunction -> ListenerFunction
runCCFunction fn ts me = fn out n vl
    where
        vl      = getCCValue me
        n       = getCCNumber me
        out     = tsOutputConnection ts

changeValue :: (ToCC a) => Connection -> CCNumber -> a -> IO ()
changeValue out knb n = send out $ MidiMessage (toCCChannel n) (CC knb (toCCValue n))

changeValue' :: (ToCC a) => (CCValue -> a) -> CCFunction
changeValue' cfn out knb v = changeValue out knb $ cfn v

changeAll ::CCFunction -> CCFunction
changeAll fn con _ val = mapM_ (\n -> fn con n val) [0..15]

cycleThroughColors :: Connection -> CCNumber -> CCValue -> IO ()
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

-- printCallback :: IORef TwisterState -> Listeners -> MidiEvent -> Env TwisterState
printCallback :: IORef TwisterState -> Listeners -> MidiEvent -> IO ()
printCallback tref ls me@(MidiEvent tm (MidiMessage ch (CC nm val))) = do
    ts <- readIORef tref
    mapM_ (\fn -> fn ts me) fns
    where
        fns = getListenersFor ls ch nm
printCallback _ _ me                                               = liftIO $ print me


test :: IO ()
test = do
    let ls  = mempty
        ls' = addCCListeners ls
            $ [ makeCCListener (\_ me  -> print me) Nothing Nothing
              , makeCCListener (\_ _   -> print "doing it on channel 1") (Just 1) Nothing
              , makeCCListener (runCCFunction $ changeAll (changeValue' KnobStrobe)) (Just 1) (Just 12)
              , makeCCListener (runCCFunction $ changeAll (changeValue' KnobPulse)) (Just 1) (Just 13)
              , makeCCListener (runCCFunction $ changeAll (changeValue' KnobBrightness)) (Just 1) (Just 15)
              , makeCCListener (runCCFunction $ changeAll (changeValue' IndicatorBrightness)) (Just 1) (Just 14) ]
            <> makeCCListenersFor (runCCFunction cycleThroughColors) (Just 1) [0..11]
            -- map (\n -> makeCCListener (runCCFunction cycleThroughColors) (Just 1) (Just n)) [0..11]

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


close :: Connection -> IO ()
close = stop



-- callback :: CDouble -> [CUChar] -> IO ()
-- callback dlt msg = putStrLn $ (foldr (showHex. fromEnum) "" msg) ++ " - " ++ (show dlt)
--
-- test :: IO ()
-- test = do
--     i <- defaultInput
--     openPort i 0 "RtMidi"
--     setCallback i callback
--     _ <- getLine
--     return ()

main :: IO ()
main = putStrLn "Hello, Haskell!"
