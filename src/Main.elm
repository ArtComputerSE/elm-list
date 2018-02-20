module Main exposing (..)

{-| An example of a sortable list using drag and drop
See the README.md file for more information
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Mouse
import Pointer
import Styles


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { isReordering : Bool
    , data : List String
    , drag : Maybe Drag
    }


init : ( Model, Cmd Msg )
init =
    { isReordering = False
    , data = initialList |> List.sort
    , drag = Nothing
    }
        ! []


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
    = ToggleReorder
    | DragStart Int Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Message: " msg of
        ToggleReorder ->
            { model | isReordering = not model.isReordering } ! []

        DragStart idx pos ->
            { model
                | drag = Just <| Drag idx pos.y pos.y
            }
                ! []

        DragAt pos ->
            { model
                | drag =
                    Maybe.map (\{ itemIndex, startY } -> Drag itemIndex startY pos.y) model.drag
            }
                ! []

        DragEnd pos ->
            case model.drag of
                Just { itemIndex, startY, currentY } ->
                    { model
                        | data =
                            moveItem
                                itemIndex
                                (calculateOffset currentY startY)
                                model.data
                        , drag = Nothing
                    }
                        ! []

                Nothing ->
                    { model
                        | drag = Nothing
                    }
                        ! []


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
        [ style Styles.pageContainer ]
        [ div
            [ style Styles.listHeader ]
            [ h3
                [ style Styles.headerTitle ]
                [ text "Sortable favorite movies" ]
            , toggleButton model
            ]
        , ul
            [ style Styles.listContainer ]
          <|
            List.indexedMap (itemView model) model.data
        ]


toggleButton : Model -> Html Msg
toggleButton model =
    let
        buttonTxt =
            if model.isReordering then
                "Reordering"
            else
                "Click to reorder"
    in
    button [ onClick ToggleReorder ] [ text buttonTxt ]


itemView : Model -> Int -> String -> Html Msg
itemView model idx item =
    let
        buttonStyle =
            if model.isReordering then
                [ ( "display", "inline-block" ) ]
            else
                [ ( "display", "none" ) ]

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
                        , ( "transition", "transform 200ms ease-in-out" )
                        ]
                    else if (idx > itemIndex) && distance currentY startY > (idx - itemIndex) * 50 - 20 then
                        [ ( "transform", "translateY(-50px)" )
                        , ( "transition", "transform 200ms ease-in-out" )
                        ]
                    else if idx /= itemIndex then
                        [ ( "transition", "transform 200ms ease-in-out" ) ]
                    else
                        []

                Nothing ->
                    []
    in
    li [ style <| Styles.listItem ++ moveStyle ++ makingWayStyle ]
        [ div [ style Styles.itemText ] [ text item ]
        , div
            [ style buttonStyle
            , Pointer.onDown (pagePos >> DragStart idx)
            , Pointer.onMove (pagePos >> DragAt)
            , Pointer.onUp (pagePos >> DragEnd)
            , Html.Attributes.style [ ( "touch-action", "none" ) ]
            ]
            [ text "drag" ]
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
