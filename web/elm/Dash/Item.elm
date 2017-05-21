module Dash.Item exposing ( Item
                          , init
                          , encode
                          , decoder
                          , Msg
                          , update
                          , view
                          , subs
                          )

-- IMPORTS ---------------------------------------------------------------------

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dom exposing (focus)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE

import Task

import Keyboard

import Maybe.Extra as Maybe_


-- MODEL -----------------------------------------------------------------------

type alias Item = { t : Maybe String
                  , u : String
                  }


-- INIT ------------------------------------------------------------------------

-- TODO: do we want it to be a checklist? or something
init : Item
init = { t = Nothing
       , u = "new item"
       }


-- JSON ------------------------------------------------------------------------

encode : Item -> JE.Value
encode {u} = JE.string u

decoder : Decoder Item
decoder = JD.string |> JD.map (Item Nothing)


-- HELPERS ---------------------------------------------------------------------

edit : Item -> Item
edit i
  = case i.t of
      Nothing -> { i | t = Just i.u }
      Just t_ -> { i | t = Nothing
                    , u = t_
                    }

write : String -> Item -> Item
write t_ i = { i | t = i.t |> Maybe.map (always t_) }


-- UPDATE ----------------------------------------------------------------------

type Msg = NoOp
         | KeyUp     Int
         | KeyDown   Int
         | ItemWrite String

-- TODO: the route should find the according spot in the queue nest 
update : Msg -> Item -> (Item, Cmd Msg)
update msg item =
  let focus_ : List (Cmd Msg)
      focus_ = focus item.u
             |> Task.attempt (always NoOp)
             |> List.singleton
      keyup_     : Int -> Item -> Item
      keyup_   c = case c of
                     16 -> identity -- shift
                     _  -> identity
      keydown_   : Int -> Item -> Item
      keydown_ c = case c of
                     13 -> edit     -- enter
                     16 -> identity -- shift
                     32 -> identity -- space
                     37 -> identity -- left  arrow
                     38 -> identity -- up    arrow
                     39 -> identity -- right arrow
                     40 -> identity -- down  arrow
                     _  -> identity
  in  case msg of
        NoOp         -> item               |> flip (!) []
        KeyUp     c_ -> item |> keyup_   c_ |> flip (!) []
        KeyDown   c_ -> item |> keydown_ c_ |> flip (!) focus_
        ItemWrite t_ -> item |> write    t_ |> flip (!) []

subs : Item -> Sub Msg
subs _
  = Sub.batch
    [ Keyboard.ups   KeyUp  
    , Keyboard.downs KeyDown
    ]

-- UPDATE ----------------------------------------------------------------------

view : Item -> Html Msg
view i = div []
         [ case i.t of
             Just t_ -> textarea [ id i.u, size <| String.length t_, value t_, onInput ItemWrite ] []
             Nothing -> text i.u
         ]
