
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import WebSocket exposing (..)
import Result exposing (..)
import Debug exposing (..)
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
    , edit : String
    }

init : (Model, Cmd Msg)
init =
  ({ tasks = [], edit = "" }, Cmd.none)

-- UPDATE

type Msg
  = Prepare String
  | Create String
  | Complete Task
  | Fetch (List Task)

socket : String
socket = "ws://taysar.com:9458/task"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Prepare body ->
      ({ model | edit = body }, Cmd.none)
    Create body ->
      -- KLUDGE
      ({ model | edit = "" }, WebSocket.send socket <| "{\"done\":false,\"body\":\"" ++ body ++ "\"}")
    Complete task ->
      (model, Cmd.none)
    Fetch tasks ->
      ({ model | tasks = tasks }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model 
  -- TODO: compose fetch and parseTasks
  = WebSocket.listen socket <| (\s -> Fetch <| parseTasks s)
      
parseTasks : String -> List Task
parseTasks s
  = Result.withDefault [] 
    <| Json.decodeString decodeTasks s
        
decodeTasks : Decoder (List Task)
decodeTasks
  = Json.list 
    <| Json.oneOf
       [ Json.object2 Task
            ("done" := Json.bool)
            ("body" := Json.string)
       , Json.succeed { done = False, body = "test" }
       ]

-- VIEW

view : Model -> Html Msg
view {edit,tasks} 
  = div [] 
    [ Html.form [ onSubmit (Create edit) ] 
      [ input [ Html.Attributes.value edit, onInput Prepare ] [] 
      ]
    , ul [] <| List.map taskItem tasks
    ]
      
taskItem : Task -> Html Msg
taskItem task
  = li [] [ text task.body ]

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
