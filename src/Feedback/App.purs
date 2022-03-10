module Feedback.App where

import Prelude

import Control.Parallel (parallel, sequential)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Feedback.InnerComponent as InnerComponent
import Feedback.Types (Buffers)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import WAGS.Interpret (context, close, decodeAudioDataFromUri)

type State = { buffers :: Maybe Buffers }

data Action = Initialize

component :: forall query input output m. MonadEffect m => MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render: render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

type Slots = (canvas :: forall query. H.Slot query Void Unit)
_canvas = Proxy :: Proxy "canvas"

initialState :: forall input. input -> State
initialState _ = { buffers: Nothing }

klz :: forall r a. Array String -> IProp (class :: String | r) a
klz = HP.classes <<< map ClassName

render :: forall m. MonadEffect m => MonadAff m => State -> H.ComponentHTML Action Slots m
render { buffers } =
  HH.div [ klz [ "w-screen", "h-screen" ] ] $
    maybe
      [ HH.div [ klz [ "flex", "flex-col", "w-full", "h-full" ] ]
          [ HH.div [ klz [ "flex-grow" ] ] [ HH.div_ [] ]
          , HH.div [ klz [ "flex-grow-0", "flex", "flex-row" ] ]
              [ HH.div [ klz [ "flex-grow" ] ]
                  []
              , HH.div [ klz [ "flex", "flex-col" ] ]
                  [ HH.h1 [ klz [ "text-center", "text-3xl", "font-bold" ] ]
                      [ HH.text "Loading..." ]
                  ]
              , HH.div [ klz [ "flex-grow" ] ] []
              ]
          , HH.div [ klz [ "flex-grow" ] ] []
          ]
      ]
      ( append []
          <<< pure
          <<< flip (HH.slot_ _canvas unit) unit
          <<< InnerComponent.component
      )
      buffers

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  Initialize -> do
    audioCtx <- H.liftEffect context
    buffers <-
      H.liftAff
        $ sequential
        $
          ( map fromHomogeneous
              $ traverse (parallel <<< decodeAudioDataFromUri audioCtx)
              $ homogeneous
                  { fsharp2: "https://freesound.org/data/previews/560/560691_12581356-hq.mp3"
                  , f1: "https://freesound.org/data/previews/560/560690_12581356-hq.mp3"
                  , d2: "https://freesound.org/data/previews/560/560689_12581356-hq.mp3"
                  , e1: "https://freesound.org/data/previews/560/560688_12581356-hq.mp3"
                  , e2: "https://freesound.org/data/previews/560/560687_12581356-hq.mp3"
                  , fsharp1: "https://freesound.org/data/previews/560/560686_12581356-hq.mp3"
                  , c3: "https://freesound.org/data/previews/560/560685_12581356-hq.mp3"
                  , dsharp1: "https://freesound.org/data/previews/560/560684_12581356-lq.mp3"
                  , dsharp2: "https://freesound.org/data/previews/560/560683_12581356-hq.mp3"
                  , d1: "https://freesound.org/data/previews/560/560682_12581356-hq.mp3"
                  , g2: "https://freesound.org/data/previews/560/560681_12581356-lq.mp3"
                  , gsharp1: "https://freesound.org/data/previews/560/560680_12581356-lq.mp3"
                  , f2: "https://freesound.org/data/previews/560/560679_12581356-hq.mp3"
                  , g1: "https://freesound.org/data/previews/560/560678_12581356-hq.mp3"
                  , gsharp2: "https://freesound.org/data/previews/560/560677_12581356-hq.mp3"
                  , c1: "https://freesound.org/data/previews/560/560676_12581356-hq.mp3"
                  , c2: "https://freesound.org/data/previews/560/560675_12581356-hq.mp3"
                  , b1: "https://freesound.org/data/previews/560/560674_12581356-hq.mp3"
                  , b2: "https://freesound.org/data/previews/560/560673_12581356-hq.mp3"
                  , csharp1: "https://freesound.org/data/previews/560/560672_12581356-hq.mp3"
                  , csharp2: "https://freesound.org/data/previews/560/560671_12581356-hq.mp3"
                  , asharp1: "https://freesound.org/data/previews/560/560670_12581356-hq.mp3"
                  , asharp2: "https://freesound.org/data/previews/560/560669_12581356-hq.mp3"
                  , a1: "https://freesound.org/data/previews/560/560668_12581356-hq.mp3"
                  , a2: "https://freesound.org/data/previews/560/560667_12581356-hq.mp3"
                  }
          )
    H.liftEffect $ close audioCtx
    H.modify_
      ( _
          { buffers = Just buffers }
      )
