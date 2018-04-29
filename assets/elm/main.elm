module Main exposing (..)

--where

import Html exposing (Html, br, button, div, form, h3, input, li, table, tbody, td, text, tr, ul)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as JD exposing (field)
import Json.Encode as JE
import Phoenix.Channel
import Phoenix.Push
import Phoenix.Socket
import Platform.Cmd


-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- CONSTANTS


socketServer : String
socketServer =
    "ws://localhost:4000/socket/websocket"



-- MODEL


type Msg
    = PhoenixMsg (Phoenix.Socket.Msg Msg)
    | JoinChannel
    | LeaveChannel
    | NoOp
    | SwitchPlayer
    | ReceiveNewPlayer JE.Value


type alias Model =
    { newMessage : String
    , messages : List String
    , phxSocket : Phoenix.Socket.Socket Msg
    , activePlayer : String
    }


initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
    Phoenix.Socket.init socketServer
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "next:player" "rooms:lobby" ReceiveNewPlayer


initModel : Model
initModel =
    Model "" [] initPhxSocket "default"


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Phoenix.Socket.listen model.phxSocket PhoenixMsg



-- COMMANDS
-- PHOENIX STUFF


type alias ChatMessage =
    { next_player : String
    }


chatMessageDecoder : JD.Decoder ChatMessage
chatMessageDecoder =
    JD.map ChatMessage
        (field "next_player" JD.string)



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
                push_ =
                    Phoenix.Push.init "next:player" "rooms:lobby"

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push push_ model.phxSocket
            in
            ( { model
                | newMessage = ""
                , phxSocket = phxSocket
              }
            , Cmd.map PhoenixMsg phxCmd
            )

        ReceiveNewPlayer raw ->
            case JD.decodeValue chatMessageDecoder raw of
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
