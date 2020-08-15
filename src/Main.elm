port module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, h2, h3, br, span)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import EasterEggs as EE


---- MODEL ----


type alias Model =
    { display1: List (Html Msg)
    , display2: List (Html Msg)
    , display3: List (Html Msg)
    , display4: List (Html Msg)
    , console: List (Html Msg)
    , console_open: Bool
    , current_img_folder: String
    , loadImg: String
    , websocket_host: String
    , connected: Bool
    , eggDisplayed: Bool
    , hasVideo: Bool
    }

type alias CK_Message =
  { key: String
    , display: String
    , payload: String
  }


init : ( Model, Cmd Msg )
init =
    ( Model
    []
    []
    []
    []
    []
    False
    "asia"
    "welcome"
    ""
    False
    False
    False
    , Cmd.none )


-- PORTS
port messageReceiver : (String -> msg) -> Sub msg
port sendMessage : String -> Cmd msg

---- UPDATE ----


type Msg
    = Recv String
    | UpdateIP String
    | Connect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateIP string -> ( { model | websocket_host = string }, Cmd.none )

    Connect ->
      if not (String.isEmpty model.websocket_host) then
        ( { model | connected = True }, sendMessage model.websocket_host )
      else
        ( model, Cmd.none )

    Recv string ->
      let
          decoded = D.decodeString ckdecoder string
      in
      case decoded of
        Ok res ->
          let
              wrapIt = [ span [ HA.class ("codespan " ++ res.key ) ] [ text res.payload ] ]
          in
          case res.display of
            "1" ->
              case res.key of
                "eval" -> ( {model | display1 = model.display1 ++ wrapIt ++ [ br [] [], br [] [] ] }, Cmd.none )
                _ -> ( {model | display1 = model.display1 ++ wrapIt }, Cmd.none )

            "2" ->
              case res.key of
                "eval" -> ( {model | display2 = model.display2 ++ wrapIt ++ [ br [] [], br [] [] ] }, Cmd.none )
                _ -> ( {model | display2 = model.display2 ++ wrapIt }, Cmd.none )


            "3" ->
              if List.member res.payload ( List.map String.fromInt <| EE.magicnumbers(model.current_img_folder) ) then
                if not (EE.isVideo res.payload) then
                  ( {model | display3 = wrapIt, loadImg = res.payload
                    , display1 = model.display1 ++ [ br [] [], br [] [] ]
                    , eggDisplayed = True  }, Cmd.none )
                else
                  ( {model | display3 = wrapIt, loadImg = res.payload
                    , display1 = model.display1 ++ [ br [] [], br [] [] ]
                    , eggDisplayed = True, hasVideo = True  }, Cmd.none )
              else
                ( {model | display3 = wrapIt, eggDisplayed = False }, Cmd.none )

            "4" -> ( {model | display4 = model.display4 ++ wrapIt }, Cmd.none )

            "console" -> ( {model | console = wrapIt }, Cmd.none )

            "cmd" ->
                case res.payload of
                  "openconsole" -> ( { model | console_open = True }, Cmd.none )
                  "closeconsole" -> ( { model | console_open = False }, Cmd.none )
                  "changeimagefolder" -> ( { model | current_img_folder = res.key }, Cmd.none)
                  _ -> (model, Cmd.none)

            _ -> (model, Cmd.none)

        Err error ->
          (model, Cmd.none)





---- VIEW ----


view : Model -> Html Msg
view model =
    div [ HA.class "container"] [
          if not model.connected then
            div [ HA.class "ipinput"] [
              Html.input [ HA.id "ipinput"
                , HA.type_ "text"
                , HA.placeholder "websocket host IP.."
                , HE.onInput UpdateIP
                , HE.on "keydown" (ifIsEnter Connect) ] []
              , Html.button [ HA.class "button"
                , HE.onClick Connect  ] [ text "Connect" ]
            ]
          else
            div [HA.class "codeheadings"] [
              h2 [] [ text "λ functions" ]
              , h2 [] [ text "Stack" ]
              , h2 [] [ text "Result" ]
              , h2 [] [ text "piano functions" ]
            ]
            , ( div [HA.class "codecontainer"] ( [] ++ ( makeDisplays model 4 [ span [] [] ] ) ) )
            , div [ HA.class "imgcontainer"
              , if String.isEmpty model.current_img_folder then
                  HA.style "background-image" ("url(../images/welcome.png)" )
                else
                  HA.style "background-image" ("url(../images/" ++ model.current_img_folder ++ "/" ++ model.current_img_folder ++ ".png)" )
                ] []
            , div [ HA.class "ck_console_container"
              , if model.console_open then
                  HA.style "opacity" "1"
                else
                  HA.style "opacity" "0"
                ] [ div [ HA.class "ck_console"
                    , if model.hasVideo then
                        HA.style "width" "50%"
                      else
                        HA.style "" ""
                    ] model.console
                    , if model.eggDisplayed && ( not (EE.isVideo model.loadImg) ) then
                        Html.img [ HA.src ("../images/" ++ model.current_img_folder ++ "/" ++ model.loadImg ++ ".png") ] []
                      else
                        span [ HA.class "videocont" ] [ Html.video ( [ HA.class "vidcontainer" ] ++
                          if String.isEmpty model.current_img_folder then
                            [ HA.src "" ]
                          else
                            [ HA.src ("../images/" ++ model.current_img_folder ++ "/" ++ model.loadImg ++ ".mp4")
                            , HA.autoplay True, HA.loop True ]
                            ) [] ]
                  ]
        ]




-- HELPER FUNCTIONS

makeDisplays: Model -> Int -> List (Html Msg) -> List (Html Msg)
makeDisplays model numOfDisplays listHtml =
    if numOfDisplays == 0 then
      List.reverse listHtml
    else
      let
          appended = List.append listHtml [ div [ HA.id ("display" ++ String.fromInt numOfDisplays) ]
              (getDisplayText model numOfDisplays)
            ]
      in
      makeDisplays model (numOfDisplays-1) appended

getDisplayText : Model -> Int -> List (Html Msg)
getDisplayText model display =
    case display of
      1 -> model.display1
      2 -> model.display2
      3 -> model.display3
      4 -> model.display4 ++ [ br [] [] ]
      _ -> [ span [] [] ]

ckdecoder: D.Decoder CK_Message
ckdecoder =
    D.map3 CK_Message
      (D.field "key" D.string)
      (D.field "display" D.string)
      (D.field "payload" D.string)

ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
  D.field "key" D.string
    |> D.andThen (\key -> if key == "Enter" then D.succeed msg else D.fail "")


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  messageReceiver Recv



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
