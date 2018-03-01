module DraggableListView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pointer
import Styles


type alias State =
    { dragging : Bool
    , drag : Drag
    }


type Config msg
    = Config
        { toMsg : Message -> msg
        }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Drag =
    { itemIndex : Int
    , startY : Float
    , currentY : Float
    }


type Message
    = DragStart Int Position
    | DragAt Position
    | DragEnd Position



-- Init


init : State
init =
    { dragging = False
    , drag = { itemIndex = 0, startY = 0.0, currentY = 0.0 }
    }



-- Update


update : Message -> State -> State
update message state =
    case message of
        DragStart idx pos ->
            { state | dragging = True, drag = Drag idx pos.y pos.y }

        DragAt pos ->
            { state | drag = newDrag state.drag pos.y }

        DragEnd pos ->
            { state | dragging = False }


newDrag : Drag -> Float -> Drag
newDrag drag newY =
    Drag drag.itemIndex drag.startY newY



-- View


view : (Message -> msg) -> State -> List a -> Html msg
view toMsg state list =
    div []
        [ toString state |> text
        , div []
            (viewItems toMsg state list)
        ]


viewItems : (Message -> msg) -> State -> List a -> List (Html msg)
viewItems toMsg state list =
    List.indexedMap (viewItem toMsg state) list


viewItem : (Message -> msg) -> State -> Int -> a -> Html msg
viewItem messageDecorator state index a =
    div
        (itemEvents messageDecorator state index)
        [ toString a |> text ]


itemEvents : (Message -> msg) -> State -> Int -> List (Attribute msg)
itemEvents messageDecorator state index =
    List.append
        [ style Styles.listItem
        , Pointer.onDown (pagePos >> DragStart index >> messageDecorator)
        , style [ ( "touch-action", "none" ) ]
        , dragAt state.dragging messageDecorator
        , moveStyle state index
        , makingWayStyle state index
        ]
        (dragEnd state.dragging messageDecorator)


moveStyle : State -> Int -> Attribute msg
moveStyle state currentIndex =
    if state.dragging && state.drag.itemIndex == currentIndex then
        style
            [ ( "transform", transformMove state.drag )
            , ( "box-shadow", "0 3px 6px rgba(0,0,0,0.24)" )
            , ( "will-change", "transform" )
            ]
    else
        style []


transformMove : Drag -> String
transformMove drag =
    "translateY( " ++ toString (drag.currentY - drag.startY) ++ "px) translateZ(10px)"


makingWayStyle : State -> Int -> Attribute msg
makingWayStyle state idx =
    let
        itemIndex =
            state.drag.itemIndex

        distance =
            distanceMoved state.drag

        indexDistance =
            (idx - itemIndex) * 50
    in
    if state.dragging then
        if (idx < itemIndex) && distance < indexDistance + 20 then
            transformMakeWay 50
        else if (idx > itemIndex) && distance > indexDistance - 20 then
            transformMakeWay -50
        else
            style []
    else
        style []


transformMakeWay : Int -> Attribute msg
transformMakeWay distance =
    Debug.log "transformMakeWay " style [ ( "transform", "translateY(" ++ toString distance ++ "px)" ) ]


distanceMoved : Drag -> Int
distanceMoved drag =
    round drag.currentY - round drag.startY


dragAt : Bool -> (Message -> msg) -> Attribute msg
dragAt dragging messageDecorator =
    if dragging then
        Pointer.onMove (pagePos >> DragAt >> messageDecorator)
    else
        style []


dragEnd : Bool -> (Message -> msg) -> List (Attribute msg)
dragEnd dragging messageDecorator =
    if dragging then
        [ Pointer.onUp (pagePos >> DragEnd >> messageDecorator)
        , Pointer.onLeave (pagePos >> DragEnd >> messageDecorator)
        ]
    else
        [ style [] ]


pagePos : Pointer.Event -> Position
pagePos event =
    let
        ( x, y ) =
            event.pointer.pagePos
    in
    Position x y
