{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HTwist.Types where

import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           HTwist.Utils
import           System.MIDI

class HasConnection a where
    getInputSource      :: a -> Source
    getOutputConnection :: a -> Connection

class ToCC a where
    toCCValue   :: a -> CCValue
    toCCChannel :: a -> Channel

-- |Retrieve the 'CCNumber' from a 'System.MIDI' 'MidiEvent'
getCCNumber :: MidiEvent -> CCNumber
getCCNumber (MidiEvent _ (MidiMessage _ (CC nm _))) = CCNumber nm
getCCNumber _                                       = error "not a CC"

-- |Retrieve the 'CCValue' from a 'System.MIDI' 'MidiEvent'
getCCValue :: MidiEvent -> CCValue
getCCValue (MidiEvent _ (MidiMessage _ (CC _ vl))) = CCValue vl
getCCValue _                                       = error "not a CC"

-- newtypes for CC message components. Note that the 'System.MIDI' library uses 'Int' for
-- these, so wrappers are needed
newtype Channel  = Channel { unChan :: Int } deriving (Show,Eq,Num,Hashable)
newtype CCNumber = CCNumber { unCCNum :: Int } deriving (Show,Eq,Enum,Num,Hashable)
newtype CCValue  = CCValue { unCCVal :: Int } deriving (Show,Eq,Enum,Ord,Num,Real,Integral,Hashable)

-- |Wrapper to create a System.Midi CC 'MidiMessage'' with our newtypes.
mkCC :: CCNumber -> CCValue -> MidiMessage'
mkCC knb vl = CC (unCCNum knb) (unCCVal vl)

-- |Wrapper to create a System.Midi CC 'MidiMessage' with our newtypes.
mkCCMessage :: Channel -> CCNumber -> CCValue -> MidiMessage
mkCCMessage ch nm vl = MidiMessage (unChan ch) (mkCC nm vl)

type Page = HM.HashMap Int TwisterKnob

-- |All information about the current state of the Twister
data TwisterState = TwisterState {
         tsInputSource      :: Source
        ,tsOutputConnection :: Connection
        ,tsPages            :: HM.HashMap Int Page
        ,tsNum              :: Int
    }

instance HasConnection TwisterState where
    getInputSource      = tsInputSource
    getOutputConnection = tsOutputConnection

data TwisterKnob = TwisterKnob {
         tnValue      :: CCValue
        ,tnColor      :: Color
        ,tnBrightness :: KnobColor
    } deriving (Show,Eq)

setKnobColor :: TwisterKnob -> Color -> TwisterKnob
setKnobColor tn c = tn { tnColor = c }

data Color = Blue
           | SkyBlue
           | Cyan
           | Aqua
           | Turquiose
           | Green
           | Lime
           | GreenYellow
           | Yellow
           | Lemon
           | Tangerine
           | Orange
           | Peach
           | Watermelon
           | Strawberry
           | Fuchsia
           | Violet
           | Purple
           | Indigo
    deriving (Show,Eq,Enum,Bounded)


instance ToCC Color where
    toCCValue Blue        = 1
    toCCValue SkyBlue     = 15
    toCCValue Cyan        = 24
    toCCValue Aqua        = 31
    toCCValue Turquiose   = 37
    toCCValue Green       = 49
    toCCValue Lime        = 57
    toCCValue GreenYellow = 60
    toCCValue Yellow      = 62
    toCCValue Lemon       = 63
    toCCValue Tangerine   = 65
    toCCValue Orange      = 70
    toCCValue Peach       = 76
    toCCValue Watermelon  = 85
    toCCValue Strawberry  = 87
    toCCValue Fuchsia     = 93
    toCCValue Violet      = 107
    toCCValue Purple      = 110
    toCCValue Indigo      = 114

    toCCChannel n          = 2


newtype KnobColor = KnobColor { unKnobColor :: CCValue }
    deriving (Num,Show,Eq)

newtype KnobBrightness = KnobBrightness { unKnobBrightness :: CCValue }
    deriving (Num,Show,Eq)

instance ToCC KnobBrightness where
    toCCValue     = hardLerpInts 0 127 17 47 . unKnobBrightness
    toCCChannel _ = 3

newtype KnobPulse = KnobPulse { unKnobPulse :: CCValue }
    deriving (Num,Show,Eq)

instance ToCC KnobPulse where
    toCCValue     = hardLerpInts 0 127 9 16 . unKnobPulse
    toCCChannel _ = 3

newtype KnobStrobe = KnobStrobe { unKnobStrobe :: CCValue }
    deriving (Num,Show,Eq)

instance ToCC KnobStrobe where
    toCCValue     = hardLerpInts 0 127 1 8 . unKnobStrobe
    toCCChannel _ = 3

newtype IndicatorBrightness = IndicatorBrightness { unIndicatorBrightness :: CCValue }
    deriving (Num,Show,Eq)

instance ToCC IndicatorBrightness where
    toCCValue     = hardLerpInts 0 127 65 95 . unIndicatorBrightness
    toCCChannel _ = 6

newtype IndicatorStrobe = IndicatorStrobe { unIndicatorStrobe :: CCValue }
    deriving (Num,Show,Eq)

instance ToCC IndicatorStrobe where
    toCCValue     = hardLerpInts 0 127 49 56 . unIndicatorStrobe
    toCCChannel _ = 6

newtype IndicatorPulse = IndicatorPulse { unIndicatorPulse :: CCValue }
    deriving (Num,Show,Eq)

instance ToCC IndicatorPulse where
    toCCValue     = hardLerpInts 0 127 57 64 . unIndicatorPulse
    toCCChannel _ = 6
