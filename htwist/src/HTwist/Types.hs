{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HTwist.Types where

import           HTwist.Utils
import           System.MIDI

getCCNumber :: MidiEvent -> CCNumber
getCCNumber (MidiEvent _ (MidiMessage _ (CC nm _))) = nm
getCCNumber _                                       = error "not a CC"

getCCValue :: MidiEvent -> Int
getCCValue (MidiEvent _ (MidiMessage _ (CC _ vl))) = vl
getCCValue _                                       = error "not a CC"

type Channel  = Int
type CCNumber = Int
type CCValue  = Int

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

class ToCC a where
    toCCValue   :: a -> CCValue
    toCCChannel :: a -> Int

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

data TwisterState = TwisterState {
         tsInputSource      :: Source
        ,tsOutputConnection :: Connection
        ,tsNum              :: Int
    }

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
