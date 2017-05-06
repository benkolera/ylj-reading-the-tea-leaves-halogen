module App where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Maybe (Maybe(..))
import Todos as Todos
import Pomodoro as Pomodoro
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Halogen.Component.ChildPath (cp1, cp2)

data Query a 
  = HandleTodosOutput Void a
  | HandlePomodoroOutput Void a

type Output = Void

type State = Unit

type ChildQuery = Coproduct2 Pomodoro.Query Todos.Query
type ChildSlot  = Either2 Unit Unit

app :: forall m. H.Component HH.HTML Query Unit Output m
app = H.parentComponent 
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState = unit

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
    render state = HH.section_ 
      [ HH.slot' cp1 unit Pomodoro.pomodoro unit (HE.input HandlePomodoroOutput) 
      , HH.slot' cp2 unit Todos.todos unit (HE.input HandleTodosOutput) ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Output m
    eval = case _ of
      HandleTodosOutput _ next -> do
        pure next
      HandlePomodoroOutput _ next -> do
        pure next