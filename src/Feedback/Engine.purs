module Feedback.Engine where

import Prelude

import Control.Monad.Indexed ((:*>))
import Data.Tuple.Nested (type (/\), (/\))
import Feedback.FullGraph (FullGraph, SubgraphSig)
import WAGS.Control.Functions.Graph (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (gain, speaker)
import WAGS.Run (RunAudio, RunEngine)

createFrame :: forall res. SubgraphSig /\ {} -> IxWAG RunAudio RunEngine Frame0 res () FullGraph Unit
createFrame wavs = icreate
  $ speaker { mainFader: gain 1.0 { wavs } }

-- we inject oracle and initial acc to avoid rebuilding the engine whenever we change these
piece
  :: forall acc env res
   . Monoid res
  => SubgraphSig -> (forall proof. env -> IxWAG RunAudio RunEngine proof res FullGraph FullGraph acc)
  -> ( forall proof
        . env
       -> acc
       -> IxWAG RunAudio RunEngine proof res FullGraph FullGraph acc
     )
  -> Scene env RunAudio RunEngine Frame0 res
piece wavs setup oracle = (\env -> createFrame (wavs /\ {}) :*> setup env) @!> iloop oracle
