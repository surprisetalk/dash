
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import WebSocket exposing (..)
import Json.Decode as Json exposing (..)

main 
  = Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
    
-- MODEL
    
type alias Task
  = { done : Bool
    , body : String
    }
      
type alias Model
  = { tasks : List Task
    }

init : (Model, Cmd Msg)
init =
  (Model [], Cmd.none)

-- UPDATE

type Msg
  = Create Task
  | Complete Task
  | Parse (List Task)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Create task ->
      (model, Cmd.none)
    Complete task ->
      (model, Cmd.none)
    Parse tasks ->
      (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen "ws://taysar.com:9458/task" (\_ -> Parse [])

-- VIEW

view : Model -> Html Msg
view model =
  div [] [ text "hello world" ]

-- view : Model -> Html Msg
-- view model =
--   div []
--     [ div [] (List.map viewMessage model.messages)
--     , input [onInput Input] []
--     , button [onClick Send] [text "Send"]
--     ]

-- viewMessage : String -> Html msg
-- viewMessage msg =
--   div [] [ text msg ]
