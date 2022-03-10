module Feedback.Setup where

import Prelude

import Feedback.Acc (initialAcc)
import Feedback.FullGraph (FullGraph)
import Feedback.Types (Res, Trigger, World, Acc)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))

setup :: forall proof. TriggeredScene Trigger World () -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
setup (TriggeredScene { world: { buffers }}) = do
  ichange' (Proxy :: _ "mainFader") 1.0
  pure (initialAcc buffers)