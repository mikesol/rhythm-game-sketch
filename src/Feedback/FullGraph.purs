module Feedback.FullGraph where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (D32)
import Data.Vec (Vec)
import WAGS.Graph.AudioUnit (Subgraph, TGain, TPlayBuf, TSpeaker, TSubgraph)
import WAGS.Interpret (AsSubgraph)
import WAGS.WebAPI (BrowserAudioBuffer)

type SubgraphSig = Subgraph
  ()
  (Vec D32 Unit)
  ( AsSubgraph
      "buffy"
      ()
      Unit
      (Maybe (Number /\ BrowserAudioBuffer))
  )
  (Int -> Unit -> Maybe (Number /\ BrowserAudioBuffer))

type SubgraphGraph = (buffy :: TPlayBuf /\ {})

type FullGraph =
  ( speaker :: TSpeaker /\ { mainFader :: Unit }
  , mainFader :: TGain /\ { wavs :: Unit }
  , wavs ::
      TSubgraph D32 "buffy"
        ()
        (Maybe (Number /\ BrowserAudioBuffer)) /\ {}
  )
