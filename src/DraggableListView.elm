module DraggableListView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias State =
    { dragging : Bool
    }


type Config msg
    = Config
        { toMsg : msg
        }


type Message
    = Drag



-- Init


init : State
init =
    { dragging = False
    }



-- Update


update : Message -> Cmd msg
update msg =
    case msg of
        Drag ->
            Cmd.none



-- View


view : Config msg -> State -> List a -> Html msg
view (Config { toMsg }) state list =
    div []
        [ toString state |> text
        , div []
            (viewItems toMsg list)
        ]


viewItems : msg -> List a -> List (Html msg)
viewItems toMsg list =
    List.map (viewItem toMsg) list


viewItem msg a =
    div [ someStyle, onClick msg ]
        [ toString a |> text ]


someStyle =
    style [ ( "border-style", "solid" ) ]
