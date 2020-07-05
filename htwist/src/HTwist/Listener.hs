{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

type Env a = ReaderT (IORef a) IO

type ListenerFunction = (TwisterState -> MidiEvent -> Env TwisterState ()) -- IO ())
type CCFunction = (Connection -> CCNumber -> CCValue -> Env TwisterState ())

data CCListener = CCListener {
         func  :: ListenerFunction
        ,chan  :: Maybe Channel
        ,ccNum :: Maybe CCNumber
    }

getState :: Env a a
getState = asks (liftIO . readIORef) >>= id

withState :: (a -> a) -> Env a a
withState fn = do
    env <- getState
    let env' = fn env
    updateState env'
    return env'

updateState :: a -> Env a ()
updateState s' = do
    ref <- ask
    liftIO $ writeIORef ref s'
    return ()

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
runCCFunction fn ts me = do
    out <- tsOutputConnection <$> getState
    fn out n vl
    where
        vl      = getCCValue me
        n       = getCCNumber me
