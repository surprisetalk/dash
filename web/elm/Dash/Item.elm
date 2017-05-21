module Dash.Item exposing ( Item
                          , init
                          , Msg
                          , update
                          , view
                          )

-- IMPORTS ---------------------------------------------------------------------

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MODEL -----------------------------------------------------------------------

type alias Item = {}


-- INIT ------------------------------------------------------------------------

init : Item
init = {}


-- API -------------------------------------------------------------------------


-- UPDATE ----------------------------------------------------------------------

type Msg = NoOp

-- TODO: the route should find the according spot in the queue nest 
update : Msg -> Item -> (Item, Cmd Msg)
update msg item =
  case msg of
    NoOp -> item ! []


-- UPDATE ----------------------------------------------------------------------

view : Item -> Html Msg
view i = text "TODO Item.view"
