module Main exposing (..)

-- IMPORTS ---------------------------------------------------------------------

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Dict exposing (..)

import Dash.Queue as Queue 


-- MAIN ------------------------------------------------------------------------

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subs
    }


-- MODEL -----------------------------------------------------------------------

type alias Model =
  { q : Queue.Model
  }


-- INIT ------------------------------------------------------------------------

init : (Model, Cmd Msg)
init = Queue.init [] |> Model |> flip (!) []


-- UPDATE ----------------------------------------------------------------------

type Msg = QueueMsg Queue.Msg

-- TODO: the route should find the according spot in the queue nest 
update : Msg -> Model -> (Model, Cmd Msg)
update msg { q } =
  case msg of
    QueueMsg msg_ -> Queue.update msg_ q
                    |> Tuple.mapFirst Model
                    |> Tuple.mapSecond (Cmd.map QueueMsg)
               
               
-- SUBSCRIPTIONS ---------------------------------------------------------------

subs : Model -> Sub Msg
subs { q } = Sub.map QueueMsg <| Queue.subs q
    
    
-- VIEW ------------------------------------------------------------------------

-- view : Model -> Html Msg
-- view { q } = q |> Queue.view |> Html.map QueueMsg

view : Model -> Html Msg
view { q } = div []
             [ toString q |> text
             , q |> Queue.view |> Html.map QueueMsg
             ]
