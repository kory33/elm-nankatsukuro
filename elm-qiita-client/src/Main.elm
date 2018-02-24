module Main exposing (..)

import Html exposing (Html, program, text, input, div, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (..)
import Html.Events.Extra exposing (..)
import Http
import Json.Decode exposing (int, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode as Encode
import Maybe exposing (withDefault, map)

qiitaDomain =
    "https://qiita.com/api/v2/"

---- MODEL ----

{--

-- This is the complete model...not necessary :p

type alias QiitaUser =
    { description : Maybe String
    , facebookId : Maybe String
    , followeesCount : Int
    , followersCount : Int
    , gitHubLoginName : Maybe String
    , qiitaId : String
    , itemsCount : Int
    , linkedinId : Int
    , location : Maybe String
    , name : Maybe String
    , organization : Maybe String
    , permanentId : Int
    , profileImageUrl : String
    , twitterScreenName : Maybe String
    , websiteUrl : Maybe String
    }
--}

type alias QiitaUser =
    { itemsCount : Int }

decodeQiitaUser : Decoder QiitaUser
decodeQiitaUser =
    decode QiitaUser
        |> required "items_count" int

decodeQiitaUserList : Decoder (List QiitaUser)
decodeQiitaUserList =
    Json.Decode.list decodeQiitaUser

type alias Model =
    { inputName : String
    , obtainedUser : Maybe QiitaUser
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" Nothing , Cmd.none )


---- UPDATE ----


type Msg
    = InputUserName String
    | GetUser
    | QiitaUserReceived (Result Http.Error QiitaUser)

getUser userId =
    let
       url = qiitaDomain ++ "/users/" ++ userId
    in
       Http.send QiitaUserReceived <| (Http.get url decodeQiitaUser)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputUserName input ->
            ( { model | inputName = input}, Cmd.none )
        GetUser ->
            ( model, getUser model.inputName )
        QiitaUserReceived (Ok user) ->
            ( {model | obtainedUser = Just user}, Cmd.none )
        QiitaUserReceived (Err e) ->
            Debug.crash <| toString e

---- VIEW ----


view : Model -> Html Msg
view { inputName, obtainedUser } =
    let
        userToAttributeList : QiitaUser -> List (Html msg)
        userToAttributeList user = [ul []
          [ toString user.itemsCount ++ " Contributions" |> text]
          ]

        userDisplay = case obtainedUser of
            Just user -> ul [] (userToAttributeList user)
            Nothing -> ul [] []
    in
        div []
        [ input
          [ placeholder "User Name"
          , value inputName
          , onInput InputUserName
          , onEnter GetUser
          ] []
        , userDisplay
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

