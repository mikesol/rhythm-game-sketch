module Feedback.Control where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import WAGS.WebAPI (AudioContext)

data Action
  = StartAudio
  | StopAudio

type State =
  { unsubscribe :: Effect Unit
  , audioCtx :: Maybe AudioContext
  }
