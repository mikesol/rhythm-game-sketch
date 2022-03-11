module Feedback.Oracle where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Control.Monad.State (execStateT, get, lift, put)
import Data.Array (uncons)
import Data.Array as Array
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Lens (Lens', over, set, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap, wrap)
import Data.Traversable (foldl, traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Feedback.FullGraph (FullGraph)
import Feedback.Types (Acc, Key(..), KeyMap, Res, Result(..), Results, Staged, Trigger, VisualNote, World, unTriggerAudio)
import Math (abs)
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions (imodifyRes)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))

doPlays :: forall proof. Number -> Acc -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
doPlays time acc = do
  let split = Array.span (\tp -> tp.starts - time < 0.15) acc.stagedAudio
  acc2 <- execStateT
    ( traverse
        ( \tp -> do
            a <- get
            lift $ unTriggerAudio (extract acc.triggers) { buffer: tp.buffer, timeOffset: max 0.0 (tp.starts - time) }
            put (a { triggers = unwrap $ unwrapCofree acc.triggers })
        )
        (split.init)
    )
    acc
  pure (acc2 { stagedAudio = split.rest })

doFail :: Number -> Number -> Array VisualNote -> Result -> Array VisualNote /\ Result
doFail time wdw s r = rest
  /\
    if foldl (&&) true (map (isJust <<< _.keyMatch) init) then r
    else Fail
  where
  { init, rest } = Array.span (\x -> x.starts + wdw <= time) s

doFails :: Number -> Number -> Acc -> Acc
doFails time wdw acc = acc
  { results = fromHomogeneous $ map snd nw
  , stagedVisual = fromHomogeneous $ map fst nw
  }
  where
  stg = homogeneous acc.stagedVisual
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
          , stagedAudio = acc.stagedAudio <>
              [ { starts: fst nxt
                , buffer: snd $ snd nxt
                }
              ]
          , stagedVisual = over
              ( case fst $ snd nxt of
                  AKey -> prop (Proxy :: Proxy "a")
                  SKey -> prop (Proxy :: Proxy "s")
                  DKey -> prop (Proxy :: Proxy "d")
                  FKey -> prop (Proxy :: Proxy "f")
              )
              ( flip Array.snoc
                  { starts: fst nxt
                  , keyMatch: Nothing
                  }
              )
              acc.stagedVisual
          }
      )
    else acc

newStagedResults :: Key -> Number -> Number -> Number -> (forall a. Lens' (KeyMap a) a) -> Staged -> Results -> Staged /\ Results
newStagedResults ky time sysTime keytime lnz staged results =
  newStaged /\ newResults
  where
  oldResult = view lnz results
  oldStaged = view lnz staged
  stagedHead = Array.uncons oldStaged
  nsr = case stagedHead of
    Nothing -> [] /\ Fail
    Just { head, tail } ->
      if isJust head.keyMatch then oldStaged /\ oldResult
      else Array.cons (head { keyMatch = Just keytime }) tail /\
        let
          gap = abs (((sysTime / 1000.0) - time) + head.starts - (keytime / 1000.0))
        in
          if gap < 0.05 then Great
          else if gap < 0.18 then Meh
          else Fail
  -- ___ = spy "NSR" (show ky /\ nsr)
  newStaged = set lnz (fst nsr) staged
  newResults = set lnz (snd nsr) results

treatMostRecent :: Number -> Number -> Array (Number /\ Key) -> Acc -> Acc
treatMostRecent = go true
  where
  go starting time sysTime notes acc =
    case uncons notes of
      Nothing -> acc
      Just { head, tail } ->
        if Just head == acc.lastConsumed then acc
        else
          (if starting then (_ { lastConsumed = Just head }) else identity) $ go false time sysTime tail
            ( let
                sr = newStagedResults (snd head) time sysTime (fst head)
                  ( case snd head of
                      AKey -> prop (Proxy :: _ "a")
                      SKey -> prop (Proxy :: _ "s")
                      DKey -> prop (Proxy :: _ "d")
                      FKey -> prop (Proxy :: _ "f")
                  )
                  acc.stagedVisual
                  acc.results
              in
                acc
                  { stagedVisual = fst sr
                  , results = snd sr
                  }
            )

oracle
  :: forall proof
   . TriggeredScene Trigger World ()
  -> Acc
  -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
oracle
  ( TriggeredScene
      { time
      , sysTime
      , world: { upcomingNoteWindow, failWindow, mostRecent }
      }
  )
  a' =
  let
    a =
      treatMostRecent time (unwrap sysTime) mostRecent
        $ modifyAcc time upcomingNoteWindow a'
  in
    do
      acc <- doFails time failWindow <$> doPlays time a
      imodifyRes
        ( const
            { staged: acc.stagedVisual
            , results: (Endo \_ -> acc.results)
            , time: wrap time
            }
        ) $> acc
