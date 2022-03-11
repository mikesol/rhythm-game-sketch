module Feedback.Acc where

import Prelude

import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Plus (empty)
import Data.Identity (Identity(..))
import Data.Lens (_1, over, traversed)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (class Pred, D0, D1, D10, D11, D12, D13, D14, D15, D16, D17, D18, D19, D2, D20, D21, D22, D23, D24, D25, D26, D27, D28, D29, D3, D30, D31, D4, D5, D6, D7, D8, D9, d31, pred)
import Feedback.FullGraph (FullGraph)
import Feedback.Types (Acc, Buffers, Key(..), Result(..), TriggerAudio(..))
import Prim.Row (class Cons)
import Prim.Symbol (class Append)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange')
import WAGS.Graph.AudioUnit (TPlayBuf)
import WAGS.Graph.Parameter (AudioOnOff(..), _offOn)

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
  cofreeN2S _ = deferCofree
    ( \_ -> Tuple
        ( TriggerAudio
            ( \{ buffer } -> ichange' (Proxy :: _ "wav0")
                { onOff: _offOn, buffer }
            )
        )
        (Identity triggers)
    )
else instance
  ( IsSymbol wavs
  , Cons wavs (Tuple TPlayBuf {}) r' FullGraph
  , N2S n s
  , Append "wav" s wavs
  , Pred n n'
  , CofreeN2S n'
  ) =>
  CofreeN2S n where
  cofreeN2S i = deferCofree
    ( \_ -> Tuple
        ( TriggerAudio
            ( \{ buffer, timeOffset } ->
                ichange' (Proxy :: _ wavs)
                  { onOff: AudioOnOff { onOff: _offOn, timeOffset }, buffer }
            )
        )
        (Identity (cofreeN2S (pred i)))
    )

triggers :: Cofree Identity TriggerAudio
triggers = cofreeN2S d31

class N2S :: Type -> Symbol -> Constraint
class N2S n s | n -> s

instance N2S D0 "0"
instance N2S D1 "1"
instance N2S D2 "2"
instance N2S D3 "3"
instance N2S D4 "4"
instance N2S D5 "5"
instance N2S D6 "6"
instance N2S D7 "7"
instance N2S D8 "8"
instance N2S D9 "9"
instance N2S D10 "10"
instance N2S D11 "11"
instance N2S D12 "12"
instance N2S D13 "13"
instance N2S D14 "14"
instance N2S D15 "15"
instance N2S D16 "16"
instance N2S D17 "17"
instance N2S D18 "18"
instance N2S D19 "19"
instance N2S D20 "20"
instance N2S D21 "21"
instance N2S D22 "22"
instance N2S D23 "23"
instance N2S D24 "24"
instance N2S D25 "25"
instance N2S D26 "26"
instance N2S D27 "27"
instance N2S D28 "28"
instance N2S D29 "29"
instance N2S D30 "30"
instance N2S D31 "31"