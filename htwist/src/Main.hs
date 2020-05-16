{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import           Sound.RtMidi
import           Control.Monad.State
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.Monoid
import           System.MIDI
import           System.MIDI.Utility
import           Numeric
import           Foreign.C


type Channel  = Int
type CCNumber = Int
type ListenerFunction = (MidiEvent -> IO ())

type MIDIListener = MidiEvent -> IO ()

data CCListener = CCListener {
         func  :: (MidiEvent -> IO ())
        ,chan  :: Maybe Channel
        ,ccNum :: Maybe CCNumber
    }

makeCCListener
    :: (MidiEvent -> IO ())
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



-- HashMap: Maybe Channel (HashMap Maybe Num [Function])

type Listeners = HM.HashMap (Maybe Channel) (HM.HashMap (Maybe CCNumber) [ListenerFunction])

getListenersFor :: Listeners -> Channel -> CCNumber -> [ListenerFunction]
getListenersFor ls ch nm = nn <> nj <> jn <> jj
    where
        nn = fromMaybe [] $ HM.lookup Nothing ls    >>= HM.lookup Nothing
        nj = fromMaybe [] $ HM.lookup Nothing ls    >>= HM.lookup (Just nm)
        jn = fromMaybe [] $ HM.lookup (Just ch) ls  >>= HM.lookup Nothing
        jj = fromMaybe [] $ HM.lookup (Just ch) ls  >>= HM.lookup (Just nm)

-- get Nothing -> Nothing listeners
-- get Nothing -> Just x  listeners
-- get Just x  -> Nothing listeners
-- get Just x  -> Just x  listeners


selectTwister :: IO Source
selectTwister = selectInputDevice "Midi Fighter Twister" (Just "Midi Fighter Twister")

printCallback :: Listeners -> MidiEvent -> IO ()
printCallback ls me@(MidiEvent ts (MidiMessage ch (CC nm val))) = mapM_ (\fn -> fn me) fns
    where
        fns = getListenersFor ls ch nm
printCallback _ me                                             = print me

test :: IO ()
test = do
    let ls  = mempty
        ls' = addCCListeners ls
            $ [ makeCCListener (\me -> print me) Nothing Nothing
              , makeCCListener (\_ -> print "doing it on channel 1") (Just 1) Nothing
              , makeCCListener (\_ -> print "1 - 15") (Just 1) (Just 15)
              , makeCCListener (\_ -> print "15 any")  Nothing (Just 15) ]
              -- , makeCCListener (\_ -> print "doing it
    src <- selectTwister
    con <- openSource src (Just (printCallback ls'))
    start con
    _ <- getLine
    stop con


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
