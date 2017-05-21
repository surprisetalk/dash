module Dash.Queue exposing ( Model
                           , init
                           , Msg
                           , update
                           , subs
                           , view
                           )

-- IMPORTS ---------------------------------------------------------------------

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dom exposing (focus)
import WebSocket
import Keyboard
import Dict exposing (..)
import Task

import  List.Extra as List_
import Maybe.Extra as Maybe_

import Dash.Item as Item exposing (..)


-- MODEL -----------------------------------------------------------------------

type alias Route = List String

-- TODO: rather than pick, we should have a "selected" (String,Queuelet)
type alias Model = { p : Maybe String
                   , q : Queue
                   , r : Route
                   , s : Bool
                   , t : Maybe String
                   }

type alias Queue = Dict String Queuelet

type Queuelet = Queue_ Queue | Item_ Item


-- INIT ------------------------------------------------------------------------

init : Route -> Model
init r = { p = Nothing
         , q = init_
         , r = r
         , s = False
         , t = Nothing
         }

init_ : Queue
init_ = Dict.empty
  
item : Item -> Queuelet
item = Item_
       
queue : Queue -> Queuelet
queue = Queue_
        
        
-- MODEL -----------------------------------------------------------------------

setQueue : Model -> Queue -> Model
setQueue model q_ = { model | q = q_ }

map : (Queue -> Queue) -> (Item -> Item) -> Model -> Model
map f g model = model.q
              |> find_ model.r
              |> Maybe.map (map__ (f >> queue) (g >> item))
              |> Maybe.map (put_ model.r model.q)
              |> Maybe.withDefault model.q
              |> setQueue model
                
find : Model -> Maybe Queuelet
find {r,q} = q |> find_ r
                    
put : Model -> Queuelet -> Model
put model = put_ model.r model.q >> setQueue model

edit : Model -> Model
edit model
  = case ( find model, model.p, model.t ) of
      ( Just _,       _, Nothing ) -> { model | t = model.p }
      ( Just _, Just p_, Just t_ ) -> { model | t = Nothing
                                             , p = model.t
                                             , q = model.q |> edit_ model.r p_ t_
                                             } 
      _                            -> { model | t = Nothing }

new : Model -> Model
new model = let e_ : Queuelet
                e_ = if   model.s 
                     then queue      init_
                     else item  Item.init
                k__ : String
                k__ = case e_ of
                        Queue_ _ -> "queue"
                        Item_  _ -> "item"
            in  case ( model.t, find model ) of
                  ( Nothing, Just (Queue_ q) ) -> q |> insert_ (q |> Dict.size |> toString |> flip (++) " " |> (++) k__) e_ |> queue |> put model
                  _                            -> model
                   
asc : Model -> Model
asc model = let r_ : List String
                r_ = model.r |> List_.init |> Maybe.withDefault []
                p_ : Maybe String
                p_ = model.r |> List_.last
            in  { model | r = r_, p = p_ }
                   
desc : Model -> Model
desc model =
  case model.p of
    Just p_ -> { model | p = Nothing, r = model.r ++ [ p_ ] }
    Nothing -> model

pick : String -> Model -> Model
pick p_ model = { model | p = Just p_ }

unpick : Model -> Model
unpick model = { model | p = Nothing }

-- TODO: if not model.t
up : Model -> Model
up model = let keys : List String
               keys = find model
                    |> Maybe_.unwrap [] (map__ Dict.keys (always []))
           in  keys
               |> List_.splitWhen (\k -> model.p |> Maybe.map ((==) k) |> Maybe.withDefault False)
               |> Maybe.map Tuple.first
               |> Maybe.andThen List_.last
               |> Maybe_.orElse (keys |> List_.last)
               |> Maybe_.unwrap model (flip pick model)
              
-- TODO: if not model.t
down : Model -> Model
down model = let keys : List String
                 keys = find model
                      |> Maybe_.unwrap [] (map__ Dict.keys (always []))
             in  keys
                 |> List_.splitWhen (\k -> model.p |> Maybe.map ((==) k) |> Maybe.withDefault False)
                 |> Maybe.map Tuple.second
                 |> Maybe.andThen (List_.getAt 1)
                 |> Maybe_.orElse (keys |> List.head)
                 |> Maybe_.unwrap model (flip pick model)
                      
shift : Model -> Model
shift model = { model | s = True }

unshift : Model -> Model
unshift model = { model | s = False }

rename : String -> Model -> Model
rename k_ model = { model | t = Just k_ }
       
  
  
-- QUEUE -----------------------------------------------------------------------
   
find_ : List String -> Queue -> Maybe Queuelet
find_ r q = case r of
              []       -> q |> queue |> Just
              k_ :: r_ -> case get_ k_ q of
                           Just (Queue_ q_) -> q_ |> find_ r_
                           _                -> Nothing

put_ : List String -> Queue -> Queuelet -> Queue
put_ r q e = case (e, r) of
               ( Queue_ q_,       [] ) ->                (          q_        )
               (  Item_ i_, k_ :: [] ) -> q |> insert_ k_ (          i_ |> item )
               (         _, k_ :: r_ ) -> q |> insert_ k_ (put_ r_ q e  |> queue)
               _                       -> q
             
get_ : String -> Queue -> Maybe Queuelet
get_ = Dict.get

insert_ : String -> Queuelet -> Queue -> Queue
insert_ = Dict.insert
          
remove_ : String -> Queue -> Queue
remove_ = Dict.remove
          
edit_ : List String -> String -> String -> Queue -> Queue
edit_ r p_ t_ q
  = case ( get_ p_ q, r ) of
      ( Just (       e_),       [] ) -> q  |> insert_  t_ e_ |> remove_ p_ 
      ( Just (Queue_ q_),  _ :: r_ ) -> q_ |> edit_ r_ p_ t_
      _                              -> q
          
-- QUEUELET  -----------------------------------------------------------------------

map__ : (Queue -> a) -> (Item -> a) -> Queuelet -> a
map__ f g e
    = case e of
        Queue_ q_ -> f q_
        Item_  i_ -> g i_
                                      
                                      
-- UPDATE ----------------------------------------------------------------------

type Msg = ItemMsg     Item.Msg
         | QueueRemove String 
         | QueueInsert String Queuelet 
         | QueueRename String 
         | QueueDesc   String 
         | QueueAsc
         | KeyDown     Int
         | KeyUp       Int
         | NoOp
         
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let e : Maybe Queuelet
      e = find model
      focus_ : List (Cmd Msg)
      focus_ = model.p
             |> Maybe_.unwrap Cmd.none
               (\p -> focus p |> Task.attempt (always NoOp))
             |> List.singleton
      keyup_     : Int -> Model -> Model
      keyup_   c = case c of
                     16 -> unshift  -- shift
                     _  -> identity
      keydown_   : Int -> Model -> Model
      keydown_ c = case c of
                     13 -> edit     -- enter
                     16 -> shift    -- shift
                     32 -> new      -- space
                     37 -> asc      -- left  arrow
                     38 -> up       -- up    arrow
                     39 -> desc     -- right arrow
                     40 -> down     -- down  arrow
                     _  -> identity
  in  case ( e, msg ) of
        ( Just (Item_  i_), ItemMsg     msg_  ) -> Item.update msg_ i_
                                                |> Tuple.mapFirst  (item >> put model)
                                                |> Tuple.mapSecond (Cmd.map ItemMsg)
        ( Just (Queue_  _), QueueRemove k_    ) -> model |> map (remove_ k_   ) identity |> flip (!) []
        ( Just (Queue_  _), QueueInsert k_ e_ ) -> model |> map (insert_ k_ e_) identity |> flip (!) []
        ( Just (Queue_  _), QueueRename k_    ) -> model |> rename       k_              |> flip (!) []
        ( Just (Queue_  _), QueueDesc   k_    ) -> model |> pick         k_       |> desc |> flip (!) []
        ( Just          _ , QueueAsc          ) -> model |> asc                          |> flip (!) []
        (               _ , KeyUp       c_    ) -> model |> keyup_       c_              |> flip (!) []
        (               _ , KeyDown     c_    ) -> model |> keydown_     c_              |> flip (!) focus_
        _                                       -> model                                |> flip (!) []


-- SUBSCRIPTIONS ---------------------------------------------------------------

subs : Model -> Sub Msg
subs model
  = Sub.batch
    [ Keyboard.ups   KeyUp  
    , Keyboard.downs KeyDown
    ]
               

-- VIEW ------------------------------------------------------------------------

blank : Html Msg
blank = text "TODO Queue.blank"

view : Model -> Html Msg
view model = model
           |> find
           |> Maybe_.unwrap blank
             (map__ (view_ model.t model.p) (Item.view >> Html.map ItemMsg))

view_ : Maybe String -> Maybe String -> Queue -> Html Msg
view_ t p = let view__   : String -> Html Msg
                view__ k = let isSelected : Bool
                               isSelected = p |> Maybe_.unwrap False ((==) k)
                               k_ : String
                               k_ = if isSelected then k ++ "*" else k
                           in  case ( isSelected, t ) of
                                 (True, Just t_) -> li [] [ input [ id k, value t_, onInput QueueRename ] [] ]
                                 _               -> li [] [ a [ onClick <| QueueDesc k ] [ text k_ ] ]
                                                               
            in  Dict.keys
             >>  List.map view__
             >>  ul []
          

