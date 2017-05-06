module Todos where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Maybe (Maybe(..))

data Query a = Toggle Int a

type Todo = 
  { completed :: Boolean
  , title     :: String 
  , todoId    :: Int
  }

type State = 
  { todos :: Array Todo }

todos :: forall m. H.Component HH.HTML Query Unit Void m
todos =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { todos : 
    [ { completed : false, title : "Write Talk", todoId : 2 }
    , { completed : true, title : "Propose Talk", todoId : 1 }
    ]}

  renderTodo :: Todo -> H.ComponentHTML Query
  renderTodo t = HH.li 
    [ HP.classes (if t.completed then [H.ClassName "completed"] else [])] 
    [ HH.label []
      [ HH.input 
        [ HP.type_ HP.InputCheckbox
        , HP.class_ (H.ClassName "toggle")
        , HE.onClick (HE.input_ $ Toggle t.todoId)
        ]
      , HH.text t.title 
      ]]

  render :: State -> H.ComponentHTML Query
  render state = HH.section_ 
    [ HH.section [HP.class_ (H.ClassName "todo")]
      [ HH.ul [HP.class_ (H.ClassName "todo-list")]
        $ map renderTodo state.todos 
        ]]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Toggle tId next -> do
      H.modify 
        (\s -> s 
          { todos = map (\t -> 
            if t.todoId == tId 
            then t { completed = not t.completed } 
            else t 
          ) s.todos }
        )
      pure next
