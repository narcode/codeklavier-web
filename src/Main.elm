port module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, h2, span)
import Html.Attributes as HA
import Json.Decode as D


---- MODEL ----


type alias Model =
    { display1: List (Html Msg)
    , display2: List (Html Msg)
    , display3: List (Html Msg)
    , display4: List (Html Msg)
    }

type alias CK_Message =
  { key: String
    , display: String
    , payload: String
  }


init : ( Model, Cmd Msg )
init =
    ( { display1 = [], display2 = [], display3 = [], display4 = [] }, Cmd.none )


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
            "1" -> ( {model | display1 = model.display1 ++ wrapIt }, Cmd.none )
            "2" -> ( {model | display2 = model.display2 ++ wrapIt }, Cmd.none )
            "3" -> ( {model | display3 = model.display3 ++ wrapIt }, Cmd.none )
            "4" -> ( {model | display4 = model.display4 ++ wrapIt }, Cmd.none )
            _ -> (model, Cmd.none)

        Err error ->
          Debug.log ( "Error" ++ (Debug.toString error) )
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
          , div [ HA.class "imgcontainer", HA.style "background-image" "url(../images/AVeinberg.jpg)" ] []
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
      4 -> model.display4
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
