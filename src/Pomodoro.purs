module Pomodoro where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Maybe (Maybe(..), maybe)
import Data.Array (replicate)
import Data.Foldable (fold)
import Data.String as Str

data Query a 
  = Noop a

type Output = Void

type Timer = 
  { minutesLeft :: Int 
  , secondsLeft :: Int
  }

type State =
  { timer :: Maybe Timer }


padDigits :: Int -> String
padDigits i = fold (replicate(max 0 (2 - Str.length intStr)) "0") <> intStr
  where
    intStr     = show i

pomodoro :: forall m. H.Component HH.HTML Query Unit Output m
pomodoro = H.component 
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState = { timer : Just { minutesLeft : 25, secondsLeft: 0 }}

    render :: State -> H.ComponentHTML Query
    render state = HH.section [HP.class_ (H.ClassName "pomodoro")]
      [ maybe renderTimerSelector renderTimer state.timer ]

    renderTimerSelector :: H.ComponentHTML Query
    renderTimerSelector = HH.section [HP.class_ (H.ClassName "timer-selector")]
      [ HH.div [HP.class_ (H.ClassName "timer-selector-msg")]
        [ HH.text "Please select a task below"]
        ]
    
    renderTimer :: Timer -> H.ComponentHTML Query
    renderTimer timer = HH.section [HP.class_ (H.ClassName "timer")]
      [ HH.span [HP.class_ (H.ClassName "timer-minutes")] [HH.text $ padDigits timer.minutesLeft] 
      , HH.span [HP.class_ (H.ClassName "timer-colon")] [HH.text ":"]
      , HH.span [HP.class_ (H.ClassName "timer-seconds")] [HH.text $ padDigits timer.secondsLeft]
      ]

    eval :: Query ~> H.ComponentDSL State Query Output m
    eval = case _ of
      Noop next -> do
        pure next
