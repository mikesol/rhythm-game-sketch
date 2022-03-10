module Feedback.Oracle where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Lens (Lens', over, set, view)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap, wrap)
import Data.Set (Set, delete, filter, findMin, insert, size)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Feedback.FullGraph (FullGraph)
import Feedback.Types (Acc, Key(..), KeyMap, Note, Res, Result(..), Trigger(..), World, unTriggerAudio)
import Math (abs)
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions (imodifyRes)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))

actOn
  :: forall proof
   . Number
  -> Acc
  -> (forall a. Lens' (KeyMap a) a)
  -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
actOn time acc lnz = out
  where
  possible = view lnz acc.staged
  out = case findMin possible of
    Nothing -> pure (acc { results = set lnz Fail acc.results })
    Just a ->
      let
        gap = abs (fst a - time)
        bf = unTriggerAudio (extract acc.triggers) { buffer: snd a }
      in
        bf $> acc
          { staged = set lnz (delete a possible) acc.staged
          , results = set lnz
              ( if gap < 0.05 then Great
                else if gap < 0.18 then Meh
                else Fail
              )
              acc.results
          , triggers = unwrap $ unwrapCofree acc.triggers
          }

doFail :: Number -> Number -> Set Note -> Result -> Set Note /\ Result
doFail time wdw s r = pruned /\ if size pruned < size s then Fail else r
  where
  pruned = filter (\(x /\ _) -> x > time + wdw) s

doFails :: Number -> Number -> Acc -> Acc
doFails time wdw acc = acc
  { results = fromHomogeneous $ map snd nw
  , staged = fromHomogeneous $ map fst nw
  }
  where
  stg = homogeneous acc.staged
  rs = homogeneous acc.results
  nw = doFail time wdw <$> stg <*> rs

modifyAcc :: Number -> Number -> Acc -> Acc
modifyAcc time wdw acc =
  case acc.notes of
    Nil -> acc
    (nxt : rest) ->
      if fst nxt < time + wdw then modifyAcc time wdw
        ( acc
            { notes = rest
            , staged = over
                ( case fst $ snd nxt of
                    AKey -> prop (Proxy :: Proxy "a")
                    SKey -> prop (Proxy :: Proxy "s")
                    DKey -> prop (Proxy :: Proxy "d")
                    FKey -> prop (Proxy :: Proxy "f")
                )
                (insert (fst nxt /\ (snd $ snd nxt)))
                acc.staged
            }
        )
      else acc

oracle
  :: forall proof
   . TriggeredScene Trigger World ()
  -> Acc
  -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
oracle
  ( TriggeredScene
      { time
      , trigger
      , world: { upcomingNoteWindow, failWindow }
      }
  )
  a' =
  let
    a = modifyAcc time upcomingNoteWindow a'
  in
    do
      acc <- case trigger of
        Thunk -> pure (doFails time failWindow a)
        Key key ->
          let
            ao = actOn time a
          in
            case key of
              AKey -> ao (prop (Proxy :: _ "a"))
              SKey -> ao (prop (Proxy :: _ "s"))
              DKey -> ao (prop (Proxy :: _ "d"))
              FKey -> ao (prop (Proxy :: _ "f"))
      imodifyRes
        ( const
            { staged: acc.staged
            , results: (Endo \_ -> acc.results)
            , time: wrap time
            }
        ) $> acc
