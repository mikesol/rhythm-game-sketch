module Feedback.Acc where

import Prelude

import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Plus (empty)
import Data.Identity (Identity(..))
import Data.Lens (_1, over, traversed)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (class Lt, class Pred, D0, D32, d31, pred)
import Feedback.Types (Acc, Buffers, Key(..), Result(..), TriggerAudio(..))
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Create.Optionals (subgraphSingleSetter)

l2cf :: forall a. Number -> NonEmpty List (Tuple Number a) -> Cofree Identity (Tuple Number a)
l2cf n i = go 0.0 i
  where
  go x (a :| Nil) = deferCofree (\_ -> Tuple a $ Identity (go (x + n) (over (traversed <<< _1) (add (x + n)) i)))
  go x (a :| (b : c)) = deferCofree (\_ -> Tuple a $ Identity (go x (b :| c)))

initialAcc :: Buffers -> Acc
initialAcc b =
  { triggers
  , stagedVisual: mempty
  , stagedAudio: empty
  , lastConsumed: Nothing
  , results: { a: None, s: None, d: None, f: None }
  , notes: l2cf 10.0 $ (2.0 /\ AKey /\ b.c1) :|
       (2.5 /\ AKey /\ b.c1)
      : (3.0 /\ SKey /\ b.g1)
      : (3.5 /\ SKey /\ b.g1)
      : (4.0 /\ FKey /\ b.a1)
      : (4.5 /\ FKey /\ b.a1)
      : (5.0 /\ DKey /\ b.g1)
      : (6.0 /\ SKey /\ b.f1)
      : (6.5 /\ SKey /\ b.f1)
      : (7.0 /\ DKey /\ b.e1)
      : (7.5 /\ DKey /\ b.e1)
      : (8.0 /\ SKey /\ b.d1)
      : (8.5 /\ SKey /\ b.d1)
      : (9.0 /\ AKey /\ b.c1)
      : Nil
  }

class CofreeN2S (n :: Type) where
  cofreeN2S :: n -> Cofree Identity TriggerAudio

instance CofreeN2S D0 where
  cofreeN2S i = deferCofree
    ( \_ -> Tuple
        ( TriggerAudio
            ( \{ buffer, timeOffset } -> ichange' (Proxy :: _ "wavs")
                (subgraphSingleSetter i (Just (timeOffset /\ buffer)))
            )
        )
        (Identity triggers)
    )
else instance
  ( Lt n D32
  , Pred n n'
  , CofreeN2S n'
  ) =>
  CofreeN2S n where
  cofreeN2S i = deferCofree
    ( \_ -> Tuple
        ( TriggerAudio
            ( \{ buffer, timeOffset } ->
                ichange' (Proxy :: _ "wavs") (subgraphSingleSetter i (Just (timeOffset /\ buffer)))
            )
        )
        (Identity (cofreeN2S (pred i)))
    )

triggers :: Cofree Identity TriggerAudio
triggers = cofreeN2S d31
