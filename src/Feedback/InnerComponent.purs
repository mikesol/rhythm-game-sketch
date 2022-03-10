module Feedback.InnerComponent where

import Prelude

import Color (rgb)
import Data.Array (dropEnd, length)
import Data.DateTime.Instant (unInstant)
import Data.Homogeneous.Record (homogeneous)
import Data.List (List(..), foldl)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for_, traverse_)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Ref as Ref
import FRP.Behavior (sampleBy, step)
import FRP.Behavior.Time (instant)
import FRP.Event (fold, makeEvent, subscribe)
import Feedback.Control (Action(..), State)
import Feedback.Engine (piece)
import Feedback.Oracle (oracle)
import Feedback.Setup (setup)
import Feedback.Types (Buffers, Key(..), Res, Result(..), Trigger(..))
import Foreign.Object as Object
import Graphics.Canvas (clearRect, getContext2D)
import Graphics.Painting (circle, fillColor, filled, text)
import Graphics.Painting as P
import Graphics.Painting.Font (bold, font, serif)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Interpret (close, context, makeFFIAudioSnapshot)
import WAGS.Run (TriggeredRun, runNoLoop)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window (document, requestAnimationFrame)
import Web.UIEvent.KeyboardEvent (fromEvent, key)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

component :: forall query input output m. MonadEffect m => MonadAff m => Buffers -> H.Component query input output m
component buffers =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction buffers
        }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribe: pure unit
  , audioCtx: Nothing
  }

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName

render :: forall m. State -> H.ComponentHTML Action () m
render st = HH.div [ classes [ "w-full", "h-full" ] ]
  [ HH.div [ classes [ "flex", "flex-row", "w-full", "h-full" ] ]
      [ HH.div [ classes [ "flex-grow" ] ] [ HH.div_ [] ]
      , HH.div [ classes [ "flex-grow-0", "flex", "flex-col" ] ]
          [ HH.div [ classes [ "flex-grow", "text-2xl" ] ]
              [ HH.text "The four dots correspond to the keys A S D F." ]
          , HH.div [ classes [ "flex" ] ]
              [ HH.canvas
                  [ HP.ref (H.RefLabel "canvas")
                  , HP.width 500
                  , HP.height 500
                  ]
              ]
          , HH.div [ classes [ "flex-grow" ] ]
              case st.audioCtx of
                Nothing ->
                  [ HH.button
                      [ classes [ "text-2xl", "m-5", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ -> StartAudio ]
                      [ HH.text "Start audio" ]
                  ]
                Just _ ->
                  [ HH.button
                      [ classes [ "text-2xl", "m-5", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ -> StopAudio ]
                      [ HH.text "Stop audio" ]
                  ]
          ]
      , HH.div [ classes [ "flex-grow" ] ] []
      ]
  ]

handleAction :: forall output m. MonadEffect m => MonadAff m => Buffers -> Action -> H.HalogenM State Action () output m Unit
handleAction buffers = case _ of
  StartAudio -> do
    handleAction buffers StopAudio
    H.getHTMLElementRef (H.RefLabel "canvas") >>= traverse_ \cvs → do
      c2d <- H.liftEffect $ getContext2D (unsafeCoerce cvs)
      audioCtx <- H.liftEffect context
      ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
      let
        animationFrameEvent = makeEvent \k -> do
          w <- window
          running <- Ref.new true
          let
            ff = void $ flip requestAnimationFrame w do
              r' <- Ref.read running
              when r' do
                -- Log.info  "running"
                k Thunk
                ff
          ff
          pure $ Ref.write false running
        keyEvent = makeEvent \k -> do
          w <- window
          d <- document w
          el <- eventListener \e -> do
            for_ (fromEvent e) \kb -> do
              case key kb of
                "a" -> k AKey
                "s" -> k SKey
                "d" -> k DKey
                "f" -> k FKey
                _ -> pure unit
          addEventListener keydown el true (unsafeCoerce d)
          pure $ removeEventListener keydown el true (unsafeCoerce d)
      let
        emptyCanvasData =
          { canvases: Object.empty
          , images: Object.empty
          , videos: Object.empty
          , webcam: Nil
          }
        xpos = homogeneous { a: 100.0, s: 200.0, d: 300.0, f: 400.0 }
        emptyRes = { a: None, s: None, d: None, f: None }
      let upcomingNoteWindow = 1.0
      unsubscribe <-
        H.liftEffect
          $ subscribe
              ( runNoLoop
                  (animationFrameEvent)
                  ( { buffers
                    , upcomingNoteWindow
                    , failWindow: 0.2
                    , mostRecent: _
                    } <$> step []
                      (flip (fold (\a b -> [a]<>(dropEnd (max 0 (length b - 31)) b))) [] (sampleBy Tuple ((unInstant >>> unwrap) <$> instant) keyEvent))
                  )
                  {}
                  ffiAudio
                  (piece setup oracle)
              )
              ( \(o :: TriggeredRun Res ()) -> do
                  let
                    rslts = unwrap o.res.results emptyRes
                    targets = foldl
                      ( \b x -> b <> filled (fillColor (rgb 0 0 0)) (circle x 400.0 20.0)
                      )
                      mempty
                      xpos
                    labels = foldl
                      ( \b (Tuple x r) -> b <> text (font serif 30 bold) x 450.0 (fillColor (rgb 0 0 0))
                          ( case r of
                              None -> ""
                              Fail -> "Fail"
                              Meh -> "Meh"
                              Great -> "Great!"
                          )
                      )
                      mempty
                      (Tuple <$> xpos <*> homogeneous rslts)
                    dots = foldl
                      ( \b (Tuple x r) -> b <> foldl
                          ( \b' { starts } -> b' <> filled
                              (fillColor (rgb 100 100 100))
                              (circle x (400.0 - ((starts - unwrap (o.res.time)) * 410.0 / upcomingNoteWindow)) 10.0)
                          )
                          mempty
                          r
                      )
                      mempty
                      (Tuple <$> xpos <*> homogeneous o.res.staged)
                  clearRect c2d { height: 500.0, width: 500.0, x: 0.0, y: 0.0 }
                  P.render c2d emptyCanvasData
                    ( labels
                        <> targets
                        <> dots
                    )
              )
      H.modify_ _
        { unsubscribe = unsubscribe
        , audioCtx = Just audioCtx
        }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    H.liftEffect do
      for_ audioCtx close
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
