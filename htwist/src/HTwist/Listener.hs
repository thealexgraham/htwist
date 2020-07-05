{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module HTwist.Listener where

import           HTwist.Utils
import           HTwist.Types

import           Control.Monad.Reader
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           System.MIDI
import           System.MIDI.Utility
import           Numeric
import           Foreign.C

-- |Listener environment. Uses a ReaderT monad holding an IORef
-- to mimic the functionality of a StateT monad since we are unable to
-- persist state using StateT due to the nature of System.MIDI callbacks.
type Env a b = ReaderT (IORef a) IO b


-- TODO: use this monad environment instead of `Env`. I want to be able to
-- use this environment with different state types, provided they have a
-- connection in and out. When you create the action ListenerFunction, that is
-- when you can specify the exact state you want
type MIDIEnv a b = (HasConnection a) => ReaderT (IORef a) IO b

-- |Retrieves the current value from the IORef inside the evironment.
getState :: Env a a
getState = asks (liftIO . readIORef) >>= id

-- |Runs a function on the value in the IORef state, update the value
-- and return the result.
withState :: (a -> a) -> Env a a
withState fn = do
    env <- getState
    let env' = fn env
    updateState env'
    return env'

-- |Update the current value inside of the IORef
updateState :: a -> Env a ()
updateState s' = do
    ref <- ask
    liftIO $ writeIORef ref s'
    return ()

-- |A function that takes a 'MidiEvent' and does something within the Environment.
type ListenerFunction = (MidiEvent -> Env TwisterState ())
type CCFunction       = (Connection -> CCNumber -> CCValue -> Env TwisterState ())

type ListenerFunction' a = (HasConnection a) => (MidiEvent -> Env a ())

-- | A 'CCListener' takes a 'ListenerFunction' and the optional 'Channel'
-- and 'CCNumber' to listen on. If the channel or number is 'Nothing', the listener
-- will be triggered on any channel or number, respectively.
data CCListener = CCListener {
         func   :: ListenerFunction
        ,clChan :: Maybe Channel
        ,clNum  :: Maybe CCNumber
    }

-- |Create a 'CCListener'. Takes a 'ListenerFunction' and the optional 'Channel'
-- and 'CCNumber' to listen on. If the channel or number is 'Nothing', the listener
-- will be triggered on any channel or number, respectively..
makeCCListener
    :: ListenerFunction
    -> Maybe Channel
    -> Maybe CCNumber
    -> CCListener
makeCCListener = CCListener

-- |Adds a list of 'CCListener' to a 'Listeners' map
addCCListeners :: Listeners -> [CCListener] -> Listeners
addCCListeners ls []     = ls
addCCListeners ls (x:xs) = addCCListeners (addCCListener ls x) xs

-- |Add a 'CCListener' to the 'Listeners' map
addCCListener :: Listeners -> CCListener -> Listeners
addCCListener ls (CCListener fn ch nm) = HM.insert ch nmap' ls
    where
        nmap  = fromMaybe mempty $ HM.lookup ch ls
        nmap' = HM.insertWith (<>) nm [fn] nmap

-- |Creates 'CCListener's for a variable amount of 'CCNumber' on a specified channel
makeCCListenersFor :: ListenerFunction -> Maybe Channel ->  [CCNumber] -> [CCListener]
makeCCListenersFor fn chn = map (\n -> makeCCListener fn chn (Just n))

-- |A group of 'ListenerFunction' mapped to the respective 'Channel' and 'CCNumbers' they
-- are meant to react to.
type Listeners = HM.HashMap (Maybe Channel) (HM.HashMap (Maybe CCNumber) [ListenerFunction])

-- |Retrieve all 'ListenerFunction's for the given 'Channel' and 'CCNumber'
getListenersFor :: Listeners -> Channel -> CCNumber -> [ListenerFunction]
getListenersFor ls ch nm = nn <> nj <> jn <> jj
    where
        nn = fromMaybe [] $ HM.lookup Nothing ls    >>= HM.lookup Nothing
        nj = fromMaybe [] $ HM.lookup Nothing ls    >>= HM.lookup (Just nm)
        jn = fromMaybe [] $ HM.lookup (Just ch) ls  >>= HM.lookup Nothing
        jj = fromMaybe [] $ HM.lookup (Just ch) ls  >>= HM.lookup (Just nm)

-- |Runs a 'CCFunction' as a 'ListenerFunction'
runCCFunction :: CCFunction -> ListenerFunction
runCCFunction fn me = do
    out <- tsOutputConnection <$> getState
    fn out n vl
    where
        vl      = getCCValue me
        n       = getCCNumber me

-- |The callback for 'openSource' to call whenever a MIDI event is triggered. Takes a
-- set of listeners and calls all listeners that match up to the MIDI event.
listenerCallback :: IORef TwisterState -> Listeners -> MidiEvent -> IO ()
listenerCallback tref ls me@(MidiEvent tm (MidiMessage ch (CC nm val))) = do
    ts <- readIORef tref
    mapM_ (\fn -> runReaderT (fn me) tref) fns
    where
        fns = getListenersFor ls (Channel ch) (CCNumber nm)

printCallback _ _ me = liftIO $ print me
