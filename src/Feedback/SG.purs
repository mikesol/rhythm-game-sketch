module Feedback.SG where

import Prelude

import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Vec (fill)
import Feedback.FullGraph (SubgraphGraph, SubgraphSig)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Parameter (AudioOnOff(..), _offOn)
import WAGS.Interpret (class AudioInterpret, AsSubgraph(..))
import WAGS.Patch (ipatch)
import WAGS.WebAPI (BrowserAudioBuffer)

createFrameSub
  :: forall res audio engine
   . AudioInterpret audio engine
  => IxWAG audio engine Frame0 res () SubgraphGraph Unit
createFrameSub = ipatch
  { microphone: empty
  , mediaElement: empty
  , subgraphs: { wavs }
  , tumults: {}
  }

subFrameLoop
  :: forall res proof audio engine
   . AudioInterpret audio engine
  => Maybe (Number /\ BrowserAudioBuffer)
  -> Unit
  -> IxWAG audio engine proof res SubgraphGraph SubgraphGraph Unit
subFrameLoop Nothing _ = pure unit
subFrameLoop (Just (time /\ buf)) _ =
  ichange' (Proxy :: _ "buffy")
    { buffer: buf
    , onOff: AudioOnOff { onOff: _offOn, timeOffset: time }
    }

wavs :: SubgraphSig
wavs = CTOR.Subgraph
  ( AsSubgraph
      ( const $ SG.istart (\_ -> createFrameSub)   (SG.iloop subFrameLoop)
      )
  )
  (fill $ const Nothing)