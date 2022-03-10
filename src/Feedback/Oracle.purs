module Feedback.Oracle where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Array as Array
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Lens (Lens', _1, _2, over, set, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap, wrap)
import Data.Traversable (foldl, traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Feedback.FullGraph (FullGraph)
import Feedback.Types (Acc, Key(..), KeyMap, Note, Res, Result(..), Trigger(..), World, unTriggerAudio)
import Math (abs)
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions (imodifyRes)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))

doPlays :: forall proof. Number -> Acc -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
doPlays time acc = do
  newStaged <- traverse
    ( Array.fromFoldable >>>
        traverse \tp@(a /\ tf /\ b /\ c) ->
          if not (not tf && a - time < 0.15) then pure tp
          else do
            unTriggerAudio (extract acc.triggers) { buffer: c, timeOffset: max 0.0 (a - time) }
            pure $ a /\ true /\ b /\ c
    )
    (homogeneous acc.staged)
  pure (acc { staged = fromHomogeneous newStaged })

actOn
  :: Number
  -> Acc
  -> (forall a. Lens' (KeyMap a) a)
  -> Acc
actOn time acc lnz = out
  where
  possible = view lnz acc.staged
  out = case Array.uncons possible of
    Nothing -> acc { results = set lnz Fail acc.results }
    Just { head, tail } ->
      let
        gap = abs (fst head - time)
      in
        acc
          { staged = set lnz (Array.cons (set (_2 <<< _2 <<< _1) true head) tail) acc.staged
          , results = set lnz
              ( if gap < 0.05 then Great
                else if gap < 0.18 then Meh
                else Fail
              )
              acc.results
          , triggers = unwrap $ unwrapCofree acc.triggers
          }

doFail :: Number -> Number -> Array Note -> Result -> Array Note /\ Result
doFail time wdw s r = rest /\ if foldl (&&) true (map (view (_2 <<< _2 <<< _1)) init) then r else Fail
  where
  { init, rest } = Array.span (\(x /\ _) -> x + wdw <= time) s

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
  let
    nxt = extract acc.notes
  in
    if fst nxt < time + wdw then modifyAcc time wdw
      ( acc
          { notes = unwrap $ unwrapCofree acc.notes
          , staged = over
              ( case fst $ snd nxt of
                  AKey -> prop (Proxy :: Proxy "a")
                  SKey -> prop (Proxy :: Proxy "s")
                  DKey -> prop (Proxy :: Proxy "d")
                  FKey -> prop (Proxy :: Proxy "f")
              )
              (flip Array.snoc (fst nxt /\ false /\ false /\ (snd $ snd nxt)))
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
        Thunk -> doFails time failWindow <$> doPlays time a
        Key key ->
          let
            ao = actOn time a
          in
            pure case key of
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
