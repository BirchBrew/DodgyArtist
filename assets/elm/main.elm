module Main exposing (..)

import Html exposing (Html, br, button, div, form, h3, input, li, table, tbody, td, text, tr, ul)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode
import Json.Encode
import Phoenix.Channel
import Phoenix.Push
import Phoenix.Socket
import Platform.Cmd


-- MAIN


type alias Flags =
    { socketServer : String
    }


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type Msg
    = PhoenixMsg (Phoenix.Socket.Msg Msg)
    | JoinChannel
    | LeaveChannel
    | NoOp
    | SwitchPlayer
    | ReceiveNewPlayer Json.Encode.Value


type alias Model =
    { messages : List String
    , phxSocket : Phoenix.Socket.Socket Msg
    , activePlayer : String
    }


initModel : String -> Model
initModel socketServer =
    Model [] (initPhxSocket socketServer) "default"


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel flags.socketServer, Cmd.none )


initPhxSocket : String -> Phoenix.Socket.Socket Msg
initPhxSocket socketServer =
    Phoenix.Socket.init socketServer
        -- TODO remove this `withDebug` before going live
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "next:player" "rooms:lobby" ReceiveNewPlayer



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Phoenix.Socket.listen model.phxSocket PhoenixMsg



-- COMMANDS
-- PHOENIX STUFF


type alias ChatMessage =
    { next_player : String
    }


chatMessageDecoder : Json.Decode.Decoder ChatMessage
chatMessageDecoder =
    Json.Decode.map ChatMessage
        (Json.Decode.field "next_player" Json.Decode.string)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
            )

        SwitchPlayer ->
            let
                push =
                    Phoenix.Push.init "next:player" "rooms:lobby"

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push push model.phxSocket
            in
            ( { model
                | phxSocket = phxSocket
              }
            , Cmd.map PhoenixMsg phxCmd
            )

        ReceiveNewPlayer raw ->
            case Json.Decode.decodeValue chatMessageDecoder raw of
                Ok chatMessage ->
                    ( { model | activePlayer = chatMessage.next_player }
                    , Cmd.none
                    )

                Err error ->
                    ( model, Cmd.none )

        JoinChannel ->
            let
                channel =
                    Phoenix.Channel.init "rooms:lobby"

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.join channel model.phxSocket
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
            )

        LeaveChannel ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.leave "rooms:lobby" model.phxSocket
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
            )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Channels:" ]
        , div [] [ text model.activePlayer ]
        , div
            []
            [ button [ onClick JoinChannel ] [ text "Join channel" ]
            , button [ onClick LeaveChannel ] [ text "Leave channel" ]
            , button [ onClick SwitchPlayer ] [ text "Switch Player" ]
            ]
        ]
