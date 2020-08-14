port module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, h2, h3, br, span)
import Html.Attributes as HA
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
    ""
    , Cmd.none )


-- PORTS
port messageReceiver : (String -> msg) -> Sub msg


---- UPDATE ----


type Msg
    = Recv String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Recv string ->
      let
          decoded = D.decodeString ckdecoder string
      in
      case decoded of
        Ok res ->
          let
              wrapIt = [ span [ HA.class "codespan"] [ text res.payload ] ]
          in
          case res.display of
            "1" ->
              if not model.console_open then
                ( {model | display1 = model.display1 ++ wrapIt }, Cmd.none )
              else
                ( {model | console = wrapIt }, Cmd.none )

            "2" -> ( {model | display2 = model.display2 ++ wrapIt }, Cmd.none )

            "3" ->
              if List.member res.payload ( List.map String.fromInt <| EE.magicnumbers(model.current_img_folder) ) then
                ( {model | display3 = wrapIt, loadImg = res.payload }, Cmd.none )
              else
                ( {model | display3 = wrapIt }, Cmd.none )

            "4" -> ( {model | display4 = model.display4 ++ wrapIt }, Cmd.none )

            "console" -> ( {model | console = wrapIt, loadImg = "" }, Cmd.none )

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
          div [HA.class "codeheadings"] [
            h2 [] [ text "λ functions" ]
            , h2 [] [ text "Stack" ]
            , h2 [] [ text "Result" ]
            , h2 [] [ text "piano functions" ]
          ]
          , ( div [HA.class "codecontainer"] ( [] ++ ( makeDisplays model 4 [ span [] [] ] ) ) )
          , div [ HA.class "imgcontainer"
            , if String.isEmpty model.current_img_folder then
                HA.style "background-image" "url(../images/AVeinberg.jpg)"
              else
                HA.style "background-image" ("url(../images/" ++ model.current_img_folder ++ "14.png)" )
              ] []
          , div [ HA.class "ck_console_container"
            , if model.console_open then
                HA.style "opacity" "1"
              else
                HA.style "opacity" "0"
              ] [ div [ HA.class "ck_console" ] model.console ]
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
