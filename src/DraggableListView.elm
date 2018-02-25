module DraggableListView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pointer


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
            { state | dragging = True }

        DragAt pos ->
            state

        DragEnd pos ->
            { state | dragging = False }



-- View


view : (Message -> msg) -> State -> List a -> Html msg
view toMsg state list =
    div []
        [ toString state |> text
        , div []
            (viewItems toMsg state.dragging list)
        ]


viewItems : (Message -> msg) -> Bool -> List a -> List (Html msg)
viewItems toMsg dragging list =
    List.indexedMap (viewItem toMsg dragging) list


viewItem : (Message -> msg) -> Bool -> Int -> a -> Html msg
viewItem messageDecorator dragging index a =
    div
        (itemEvents messageDecorator dragging index)
        [ toString a |> text ]


itemEvents : (Message -> msg) -> Bool -> Int -> List (Attribute msg)
itemEvents messageDecorator dragging index =
    List.append
        [ someStyle
        , Pointer.onDown (pagePos >> DragStart index >> messageDecorator)
        , style [ ( "touch-action", "none" ) ]
        , dragAt dragging messageDecorator
        ]
        (dragEnd dragging messageDecorator)


dragEnd dragging messageDecorator =
    if dragging then
        [ Pointer.onUp (pagePos >> DragEnd >> messageDecorator)
        , Pointer.onLeave (pagePos >> DragEnd >> messageDecorator)
        ]
    else
        [ style [] ]


dragAt dragging messageDecorator =
    if dragging then
        Pointer.onMove (pagePos >> DragAt >> messageDecorator)
    else
        style []


someStyle =
    style [ ( "border-style", "solid" ) ]


pagePos : Pointer.Event -> Position
pagePos event =
    let
        ( x, y ) =
            event.pointer.pagePos
    in
    Position x y
