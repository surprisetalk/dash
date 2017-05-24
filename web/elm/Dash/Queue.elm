
port module Dash.Queue exposing ( Model
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
import Keyboard
import Dict exposing (Dict)
import Task

import Json.Encode as JE
import Json.Decode as JD exposing (Decoder)

import   List.Extra as List_
import  Maybe.Extra as Maybe_
import Result.Extra as Result_

import Phoenix.Socket

import Dash.Item as Item exposing (..)


-- PORTS -----------------------------------------------------------------------

port queueSub : (JE.Value -> msg) -> Sub msg

port queuePub : JE.Value -> Cmd msg


-- MODEL -----------------------------------------------------------------------

type alias Route = List String

-- TODO: rather than pick, we should have a "selected" (String,Queuelet)
type alias Model = { p : Maybe String
                   , q : Queue
                   , r : Route
                   , s : Bool
                   , t : Maybe String
                   , v : Maybe Int
                   }

type alias Queue = List (String, Queuelet)

type Queuelet = Queue_ Queue | Item_ Item


-- INIT ------------------------------------------------------------------------

init : Route -> (Model, Cmd Msg)
init r = let model = { p = Nothing
                     , q = init_
                     , r = r
                     , s = False
                     , t = Nothing
                     , v = Nothing
                     }
         in  ( model, model |> encode |> queuePub )

init_ : Queue
init_ = []
  
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

-- TODO: prevent from overwriting collisions
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
                        Queue_ _ -> "queue "
                        Item_  _ -> "item "
            in  case ( model.t, find model ) of
                  ( Nothing, Just (Queue_ q) ) -> q |> insert_ (q |> List.length |> toString |> (++) k__) e_ |> queue |> put model
                  _                            -> model

inc : Model -> Model
inc model = { model | v = model.v |> Maybe.map ((+) 1) }
                   
asc : Model -> Model
asc model = let r_ : List String
                r_ = model.r |> List_.init |> Maybe.withDefault []
                p_ : Maybe String
                p_ = model.r |> List_.last
            in  if   Maybe_.isJust model.t
                then   model
                else { model | r = r_
                             , p = p_
                             , t = Nothing
                             }
                   
desc : Model -> Model
desc model =
  case ( model.t, model.p ) of
    ( Just  _,       _ ) ->   model
    (       _, Nothing ) ->   model
    ( Nothing, Just p_ ) -> { model | p = Nothing
                                   , r = model.r ++ [ p_ ]
                                   , t = Nothing
                                   }

pick : String -> Model -> Model
pick p_ model = { model | p = Just p_ }

unpick : Model -> Model
unpick model = { model | p = Nothing }

-- TODO: List_.swapAt
up : Model -> Model
up model = if   Maybe_.isJust model.t
           then model
           else let keys : List String
                    keys = find model
                         |> Maybe_.unwrap [] (map__ (List.map Tuple.first) (always []))
                in  keys
                    |> List_.splitWhen (\k -> model.p |> Maybe.map ((==) k) |> Maybe.withDefault False)
                    |> Maybe.map Tuple.first
                    |> Maybe.andThen List_.last
                    |> Maybe_.orElse (keys |> List_.last)
                    |> Maybe_.unwrap { model | t = Nothing } (flip pick { model | t = Nothing })
              
-- TODO: List_.swapAt
down : Model -> Model
down model = if   Maybe_.isJust model.t
             then model
             else let keys : List String
                      keys = find model
                           |> Maybe_.unwrap [] (map__ (List.map Tuple.first) (always []))
                  in  keys
                      |> List_.splitWhen (\k -> model.p |> Maybe.map ((==) k) |> Maybe.withDefault False)
                      |> Maybe.map Tuple.second
                      |> Maybe.andThen (List_.getAt 1)
                      |> Maybe_.orElse (keys |> List.head)
                      |> Maybe_.unwrap { model | t = Nothing } (flip pick { model | t = Nothing })
                      
shift : Model -> Model
shift model = { model | s = True }

unshift : Model -> Model
unshift model = { model | s = False }

rename : String -> Model -> Model
rename k_ model = { model | t = Just k_ }

remove : Model -> Model
remove model = case ( model.s, model.p ) of
                 ( True, Just p_ ) -> model |> map (remove_ p_) identity
                 _                 -> model
  
  
-- QUEUE -----------------------------------------------------------------------
   
find_ : List String -> Queue -> Maybe Queuelet
find_ r q = case r of
              []       -> q |> queue |> Just
              k_ :: [] -> get_ k_ q
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
get_ k = Dict.fromList >> Dict.get k

-- (+:) : List a -> a -> List a
-- (+:) xs = (++) xs << List.singleton

-- (<<<) : (c -> d) -> (a -> b -> c) -> a -> b -> d
-- (<<<) = (<<) (<<) (<<)

insert_ : String -> Queuelet -> Queue -> Queue
insert_ k_ e_ q = let a_ = (k_,e_)
                  in  case q of
                        [] -> [ a_ ]
                        a :: b -> if   k_ /= Tuple.first a
                                 then a  :: insert_ k_ e_ b
                                 else a_ ::               b
          
remove_ : String -> Queue -> Queue
remove_ k = List_.filterNot <| (==) k << Tuple.first
          
edit_ : List String -> String -> String -> Queue -> Queue
edit_ r p_ t_ q
  = case ( find_ r q ) of
      Nothing          -> q
      Just (Item_   _) -> q
      Just (Queue_ q_) -> case get_ p_ q_ of
                           Just e_ -> q_ |> List_.replaceIf (Tuple.first >> (==) p_) (t_,e_) |> queue |> put_ r q
                           Nothing -> q
          
-- QUEUELET  -----------------------------------------------------------------------

map__ : (Queue -> a) -> (Item -> a) -> Queuelet -> a
map__ f g e
    = case e of
        Queue_ q_ -> f q_
        Item_  i_ -> g i_
                                      

-- JSON ------------------------------------------------------------------------

(:=) : String -> Decoder a -> Decoder a
(:=) = JD.field

encode : Model -> JE.Value
encode {v,q} = JE.object
               [ "version" => (v |> Maybe.withDefault 0 |> JE.int)
               , "queue"   => encode_ q
               ]

encode_ : Queue -> JE.Value
encode_ = (<<) JE.list <| List.map <| \(k,e) -> JE.object [ "k" => JE.string k, "v" => map__ encode_ Item.encode e ]

decode : Model -> JD.Value -> Result String Model
decode model = JD.decodeValue <| decoder model

decoder : Model -> Decoder Model
decoder model = JD.map2 (\v_ q_ -> {model | v = Just v_, q = q_ })
                ("version" := JD.int  )
                ("queue"   := decoder_)

decoder_ : Decoder Queue
decoder_ = JD.list
         <| JD.map2 (,)
           ("k" := JD.string)
           ("v" := decoder__)
          
decoder__ : Decoder Queuelet
decoder__ = JD.oneOf
            [ JD.map Queue_ <| JD.lazy (\_ -> decoder_)
            , JD.map Item_  <| Item.decoder
            ]


-- UPDATE ----------------------------------------------------------------------

type Msg = PhoenixMsg  (Phoenix.Socket.Msg Msg)
         | ItemMsg      Item.Msg
         | ModelPublish
         | ModelUpdate  Model 
         | QueueRemove  String 
         | QueueInsert  String Queuelet 
         | QueueRename  String 
         | QueueDesc    String 
         | QueueAsc
         | KeyDown      Int
         | KeyUp        Int
         | NoOp
         | Log          String
         
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let e : Maybe Queuelet
      e = find model
      publish : Model -> (Model, Cmd Msg)
      publish model = if   Maybe_.isNothing model.t
                      then model ! [ model |> inc |> encode |> queuePub ]
                      else model ! [ focus_                          ]
      focus_ : Cmd Msg
      focus_ = model.p
             |> Maybe_.unwrap Cmd.none
               (\p -> focus p |> Task.attempt (always NoOp))
      keyup_     : Int -> Model -> Model
      keyup_   c = case c of
                     16 -> unshift  -- shift
                     _  -> identity
      keydown_   : Int -> Model -> (Model, Cmd Msg)
      keydown_ c = case c of
                     08 -> remove   >> publish     -- backspace
                     13 -> edit     >> publish     -- enter
                     16 -> shift    >> flip (!) [] -- shift
                     32 -> new      >> publish     -- space
                     37 -> asc      >> flip (!) [] -- left  arrow
                     38 -> up       >> flip (!) [] -- up    arrow
                     39 -> desc     >> flip (!) [] -- right arrow
                     40 -> down     >> flip (!) [] -- down  arrow
                     72 -> asc      >> flip (!) [] -- h
                     74 -> down     >> flip (!) [] -- j
                     75 -> up       >> flip (!) [] -- k
                     76 -> desc     >> flip (!) [] -- l
                     _  -> identity >> flip (!) []
 -- focus_, model |> keydown_ c_ |> inc |> encode |> queuePub 
  in  case ( e, msg ) of
        (               _ , Log          x     ) -> let x_ = Debug.log "LOG" x
                                                   in model ! []
        -- (               _ , PhoenixMsg   msg_  ) -> let (              u_  ,                    cmd_ ) = Phoenix.Socket.update msg_ model.u
        --                                            in  ({ model | u = u_ }, Cmd.map PhoenixMsg cmd_ )
        -- (               _ , QueuePublish       ) -> let (              u_  ,                    cmd_ ) = flip Phoenix.Socket.push model.u
        --                                                                                               <| Phoenix.Push.withPayload (JE.string "HULLO")
        --                                                                                               <| Phoenix.Push.init "room:lobby" "room:lobby"
        --                                            in  ({ model | u = u_ }, Cmd.map PhoenixMsg cmd_ )
        ( Just (Item_  i_), ItemMsg      msg_  ) -> Item.update msg_ i_
                                                 |> Tuple.mapFirst  (item >> put model)
                                                 |> Tuple.mapSecond (Cmd.map ItemMsg)
        (               _ , ModelUpdate  m_    ) -> m_                                    |> flip (!) []
        ( Just (Queue_  _), QueueRemove  k_    ) -> model |> map (remove_  k_   ) identity |> flip (!) []
        ( Just (Queue_  _), QueueInsert  k_ e_ ) -> model |> map (insert_  k_ e_) identity |> flip (!) []
        ( Just (Queue_  _), QueueRename  k_    ) -> model |> rename        k_              |> flip (!) []
        ( Just (Queue_  _), QueueDesc    k_    ) -> model |> pick          k_       |> desc |> flip (!) []
        ( Just          _ , QueueAsc           ) -> model |> asc                           |> flip (!) []
        (               _ , KeyUp        c_    ) -> model |> keyup_        c_              |> flip (!) []
        (               _ , KeyDown      c_    ) -> model |> keydown_      c_
        _                                        -> model                                 |> flip (!) []


-- SUBSCRIPTIONS ---------------------------------------------------------------

subs : Model -> Sub Msg
subs model
  = Sub.batch
    [ Keyboard.ups   KeyUp  
    , Keyboard.downs KeyDown
    , queueSub <| decode model >> Result_.unpack Log ModelUpdate
    -- , Phoenix.Socket.listen model.u PhoenixMsg
    , case find model of
        Just (Item_  i_) -> Sub.map ItemMsg <| Item.subs i_
        _                -> Sub.none
    ] 
               

-- VIEW ------------------------------------------------------------------------

(=>) : a -> b -> (a, b)
(=>) = (,)

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
                           in  case ( isSelected, t ) of
                                 (True, Just t_) -> li [ style <| if isSelected then [ "font-weight" => "bold" ] else [] ] [ input [ id k, size <| String.length t_, value t_, onInput QueueRename ] [] ]
                                 _               -> li [ style <| if isSelected then [ "font-weight" => "bold" ] else [] ] [ a [ onClick <| QueueDesc k ] [ text k ] ]
                                                               
            in  List.map Tuple.first
             >>  List.map view__
             >>  ul []
          

