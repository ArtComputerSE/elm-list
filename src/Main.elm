module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Mouse
import Pointer


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { data : List String
    , drag : Maybe Drag
    }


init : ( Model, Cmd Msg )
init =
    ( { data = initialList |> List.sort
      , drag = Nothing
      }
    , Cmd.none
    )


type alias Drag =
    { itemIndex : Int
    , startY : Float
    , currentY : Float
    }


initialList =
    [ "Shawshank Redemption"
    , "Godfather"
    , "Dark Knight"
    , "12 Angry Men"
    , "Schindler’s List"
    , "Pulp Fiction"
    , "Lord of the Rings"
    , "The Good, the Bad and the Ugly"
    , "Fight Club"
    , "The Empire Strikes Back"
    ]



-- UPDATE


type PointerEvent
    = None
    | Down Pointer.Event
    | Move Pointer.Event
    | Up Pointer.Event


type alias Position =
    { x : Float
    , y : Float
    }


type Msg
    = DragStart Int Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Message: " msg of
        DragStart idx pos ->
            ( { model | drag = Just <| Drag idx pos.y pos.y }, Cmd.none )

        DragAt pos ->
            ( { model | drag = Maybe.map (\{ itemIndex, startY } -> Drag itemIndex startY pos.y) model.drag }, Cmd.none )

        DragEnd pos ->
            case model.drag of
                Just { itemIndex, startY, currentY } ->
                    ( { model | data = moveItem itemIndex (calculateOffset currentY startY) model.data, drag = Nothing }, Cmd.none )

                Nothing ->
                    ( { model | drag = Nothing }, Cmd.none )


calculateOffset : Float -> Float -> Int
calculateOffset fCurrentY fStartY =
    let
        currentY =
            round fCurrentY

        startY =
            round fStartY
    in
    (currentY
        - startY
        + (if currentY < startY then
            -20
           else
            20
          )
    )
        // 50


moveItem : Int -> Int -> List a -> List a
moveItem fromPos offset list =
    let
        listWithoutMoved =
            List.take fromPos list ++ List.drop (fromPos + 1) list

        moved =
            List.take 1 <| List.drop fromPos list
    in
    List.take (fromPos + offset) listWithoutMoved
        ++ moved
        ++ List.drop (fromPos + offset) listWithoutMoved



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "pageContainer" ]
        [ div
            [ class "listHeader" ]
            [ h3
                [ class "headerTitle" ]
                [ text "Sortable favorite movies" ]
            , toggleButton model
            ]
        , div
            [ class "listContainer" ]
          <|
            List.indexedMap (itemView model) model.data
        ]


toggleButton : Model -> Html Msg
toggleButton model =
    let
        buttonTxt =
            if model.drag == Nothing then
                ""
            else
                "Reordering"
    in
    span [] [ text buttonTxt ]


itemView : Model -> Int -> String -> Html Msg
itemView model idx item =
    let
        moveStyle =
            case model.drag of
                Just { itemIndex, startY, currentY } ->
                    if itemIndex == idx then
                        [ ( "transform", "translateY( " ++ toString (currentY - startY) ++ "px) translateZ(10px)" )
                        , ( "box-shadow", "0 3px 6px rgba(0,0,0,0.24)" )
                        , ( "will-change", "transform" )
                        ]
                    else
                        []

                Nothing ->
                    []

        makingWayStyle =
            case model.drag of
                Just { itemIndex, startY, currentY } ->
                    if (idx < itemIndex) && distance currentY startY < (idx - itemIndex) * 50 + 20 then
                        [ ( "transform", "translateY(50px)" )
                        ]
                    else if (idx > itemIndex) && distance currentY startY > (idx - itemIndex) * 50 - 20 then
                        [ ( "transform", "translateY(-50px)" )
                        ]
                    else
                        []

                Nothing ->
                    []
    in
    div
        [ class "listItem"
        , style (moveStyle ++ makingWayStyle)
        , Pointer.onDown (pagePos >> DragStart idx)
        , Pointer.onMove (pagePos >> DragAt)
        , Pointer.onUp (pagePos >> DragEnd)
        , Pointer.onLeave (pagePos >> DragEnd)
        , Html.Attributes.style [ ( "touch-action", "none" ) ]
        ]
        [ div [ class "itemText" ]
            [ text item ]
        ]


distance : Float -> Float -> Int
distance fCurrentY fSTartY =
    round fCurrentY - round fSTartY


pagePos : Pointer.Event -> Position
pagePos event =
    let
        ( x, y ) =
            event.pointer.pagePos
    in
    Position x y
