module Main2 exposing (..)

import DraggableListView exposing (..)
import Html exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { data : List Int
    , state : DraggableListView.State
    }


type Msg
    = SomeMessage
    | Draggable DraggableListView.Message


init : ( Model, Cmd Msg )
init =
    ( { data = [ 1, 2, 3, 4, 5 ]
      , state = DraggableListView.init
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SomeMessage ->
            ( model, Cmd.none )

        Draggable message ->
            ( { model | state = DraggableListView.update message model.state }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Demo of Draggable List View" ]
        , DraggableListView.view config model.state model.data
        ]


config : DraggableListView.Config Msg
config =
    DraggableListView.Config
        { dragMessage = Draggable }
