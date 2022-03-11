module Feedback.Types where

import Prelude

import Control.Comonad.Cofree (Cofree)
import Data.Identity (Identity)
import Data.Maybe (Maybe)
import Data.Monoid.Additive (Additive)
import Data.Monoid.Endo (Endo)
import Data.Tuple.Nested (type (/\))
import Feedback.FullGraph (FullGraph)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine)
import WAGS.WebAPI (BrowserAudioBuffer)

type Buffers =
  { fsharp2 :: BrowserAudioBuffer
  , f1 :: BrowserAudioBuffer
  , d2 :: BrowserAudioBuffer
  , e1 :: BrowserAudioBuffer
  , e2 :: BrowserAudioBuffer
  , fsharp1 :: BrowserAudioBuffer
  , c3 :: BrowserAudioBuffer
  , dsharp1 :: BrowserAudioBuffer
  , dsharp2 :: BrowserAudioBuffer
  , d1 :: BrowserAudioBuffer
  , g2 :: BrowserAudioBuffer
  , gsharp1 :: BrowserAudioBuffer
  , f2 :: BrowserAudioBuffer
  , g1 :: BrowserAudioBuffer
  , gsharp2 :: BrowserAudioBuffer
  , c1 :: BrowserAudioBuffer
  , c2 :: BrowserAudioBuffer
  , b1 :: BrowserAudioBuffer
  , b2 :: BrowserAudioBuffer
  , csharp1 :: BrowserAudioBuffer
  , csharp2 :: BrowserAudioBuffer
  , asharp1 :: BrowserAudioBuffer
  , asharp2 :: BrowserAudioBuffer
  , a1 :: BrowserAudioBuffer
  , a2 :: BrowserAudioBuffer

  }

data Trigger = Thunk

data Key = AKey | SKey | DKey | FKey

derive instance Eq Key
derive instance Ord Key
instance Show Key where
  show AKey = "A"
  show SKey = "S"
  show DKey = "D"
  show FKey = "F"

type NoteToKey = Number /\ Key /\ BrowserAudioBuffer

type VisualNote =
  { starts :: Number
  , keyMatch :: Maybe Number
  }

type AudioNote =
  { starts :: Number
  , buffer :: BrowserAudioBuffer
  }

type World =
  { buffers :: Buffers
  , upcomingNoteWindow :: Number
  , failWindow :: Number
  , mostRecent :: Array (Number /\ Key)
  }

type Res =
  { staged :: Staged
  , results :: Endo (->) Results
  , time :: Additive Number
  }

newtype TriggerAudio = TriggerAudio
  ( forall proof
     . { buffer :: BrowserAudioBuffer, timeOffset :: Number }
    -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit
  )

unTriggerAudio
  :: TriggerAudio
  -> forall proof
   . { buffer :: BrowserAudioBuffer, timeOffset :: Number }
  -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit
unTriggerAudio (TriggerAudio ta) = ta

type KeyMap tp =
  { a :: tp
  , s :: tp
  , d :: tp
  , f :: tp
  }

data Result = None | Fail | Meh | Great

type Staged = KeyMap (Array VisualNote)
type Results = KeyMap Result

type Acc =
  { triggers :: Cofree Identity TriggerAudio
  , stagedVisual :: Staged
  , stagedAudio :: Array AudioNote
  , notes :: Cofree Identity NoteToKey
  , results :: Results
  , lastConsumed :: Maybe (Number /\ Key)
  }