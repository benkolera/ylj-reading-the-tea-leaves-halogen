module Todo where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Maybe (Maybe(..))

data Query a 
  = Toggle a 
  | UpdateTodo State a

type State = 
  { completed :: Boolean
  , title     :: String 
  , todoId    :: Int
  }

todo :: forall m. H.Component HH.HTML Query State Void m
todo =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: (\t -> Just $ UpdateTodo t unit)
    }
  where

  render :: State -> H.ComponentHTML Query
  render t = HH.li 
    [ HP.classes (if t.completed then [H.ClassName "completed"] else [])] 
    [ HH.label []
      [ HH.input 
        [ HP.type_ HP.InputCheckbox
        , HP.class_ (H.ClassName "toggle")
        , HE.onClick (HE.input_ Toggle)
        ]
      , HH.text t.title 
      ]]
  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Toggle next -> do
      H.modify (\t -> t { completed = not t.completed } )
      pure next
    UpdateTodo t next -> do
      H.put t
      pure next