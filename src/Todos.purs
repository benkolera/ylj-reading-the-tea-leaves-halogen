module Todos where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import DOM.Event.KeyboardEvent (code)
import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..))

import Todo as Todo

data Query a 
  = UpdateInput String a
  | NewTodo a

type State = 
  { todos :: Array Todo.State
  , editingStr :: String
  , nextId :: Int
  }

newtype TodoSlot = TodoSlot Todo.Id
derive instance eqTodoSlot :: Eq TodoSlot
derive instance ordTodoSlot :: Ord TodoSlot

todos :: forall m. H.Component HH.HTML Query Unit Void m
todos =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = 
    { editingStr : ""
    , nextId : 3
    , todos : 
      [ { completed : false, title : "Write Talk", todoId : 2 }
      , { completed : true, title : "Propose Talk", todoId : 1 }
      ]}

  renderTodo :: Todo.State -> H.ParentHTML Query Todo.Query TodoSlot m
  renderTodo t =
    HH.slot
        (TodoSlot t.todoId)
        Todo.todo
        t
        (const Nothing)

  render :: State -> H.ParentHTML Query Todo.Query TodoSlot m
  render state = HH.section_ 
    [ HH.section [HP.class_ (H.ClassName "todo")]
      [ HH.input 
        [ HP.placeholder "What needs to be done?"
        , HP.class_ (H.ClassName "new-todo")
        , HP.type_ HP.InputText 
        , HP.value state.editingStr
        , HE.onValueInput (HE.input UpdateInput)
        , HE.onKeyPress (\e -> 
          if code e == "Enter" then (Just $ H.action NewTodo) else Nothing
          )
        ]
      , HH.ul [HP.class_ (H.ClassName "todo-list")]
        $ map renderTodo state.todos 
      ]]

  eval :: Query ~> H.ParentDSL State Query Todo.Query TodoSlot Void m
  eval = case _ of
    UpdateInput s next -> do
      H.modify (_ { editingStr = s })
      pure next
    NewTodo next -> do
      H.modify (\s -> s 
        { editingStr = ""
        , nextId = s.nextId + 1
        , todos = 
          [{ completed : false, title : s.editingStr, todoId : s.nextId}]
          <> s.todos 
        })
      pure next

