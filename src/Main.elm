module Main exposing (Model)

import Browser
import Browser.Navigation as Nav
import Debug
import Domain exposing (Domain)
import GradeLevel exposing (GradeLevel)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import HttpBuilder exposing (..)
import Json.Decode as Decode
import Mission exposing (Mission, MissionId, unwrapId)
import RemoteData exposing (WebData)
import Routing exposing (Route(..))
import Url


type alias Model =
    { domains : WebData (List Domain)
    , gradeLevels : WebData (List GradeLevel)
    , missions : WebData (List Mission)
    , route : Maybe Route
    , key : Nav.Key
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | ChangedUrl Url.Url
    | DomainsLoadingComplete (Result Http.Error (List Domain))
    | GradeLevelsLoadingComplete (Result Http.Error (List GradeLevel))
    | MissionsLoadingComplete (Result Http.Error (List Mission))


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { gradeLevels = RemoteData.NotAsked, domains = RemoteData.NotAsked, missions = RemoteData.NotAsked, route = Routing.fromUrl url, key = key }
    , Cmd.batch
        [ Domain.fetchAll |> Http.send DomainsLoadingComplete
        , GradeLevel.fetchAll |> Http.send GradeLevelsLoadingComplete
        , Mission.fetchAll |> HttpBuilder.send MissionsLoadingComplete
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MissionsLoadingComplete (Err error) ->
            ( { model | missions = RemoteData.Failure error }, Cmd.none )

        MissionsLoadingComplete (Ok decodedValue) ->
            ( { model | missions = RemoteData.Success decodedValue }, Cmd.none )

        DomainsLoadingComplete (Err error) ->
            ( { model | domains = RemoteData.Failure error }, Cmd.none )

        DomainsLoadingComplete (Ok decodedValue) ->
            ( { model | domains = RemoteData.Success decodedValue }, Cmd.none )

        GradeLevelsLoadingComplete (Err error) ->
            ( { model | domains = RemoteData.Failure error }, Cmd.none )

        GradeLevelsLoadingComplete (Ok decodedValue) ->
            ( { model | gradeLevels = RemoteData.Success decodedValue }, Cmd.none )

        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        ChangedUrl url ->
            ( { model | route = Routing.fromUrl url }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Test App"
    , body = renderRoute model
    }



-- Let's discuss this
-- It time, provide an example


renderRoute : Model -> List (Html msg)
renderRoute model =
    case model.route of
        Just CurriculumRoute ->
            [ renderCurriculum model
            ]

        Just (MissionRoute missionId) ->
            [ renderMission model missionId ]

        Nothing ->
            [ h2 [] [ text "Error!, no route found" ] ]


renderMission : Model -> MissionId -> Html msg
renderMission model missionId =
    let
        findMission missions =
            missions
                |> List.filter (\m -> m.id == missionId)
                |> List.head
    in
    case model.missions of
        RemoteData.NotAsked ->
            text "YOU FAIL"

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Failure err ->
            text (Debug.toString err)

        RemoteData.Success missions ->
            case findMission missions of
                Just aMission ->
                    div []
                        -- [ div [] [ text (Debug.toString aMission) ]
                        [ div []
                            [ h1 []
                                [ text "Mission" ]
                            , p []
                                [ text <| "mission_id: " ++ (Mission.idToString aMission.id) ]
                            , p []
                                [ text  <| "help_text: " ++ aMission.helpText ]
                            , p []
                                [ text  <| "active: "  ++ (Debug.toString aMission.active)]
                            ]
                        , Html.form []
                            [ input [ name "id", type_ "hidden", value <| Mission.idToString aMission.id ]
                                []
                            , textarea [ name "help_text" ]
                                [ text aMission.helpText ]
                            , input [ name "active", type_ "checkbox", value "true", checked aMission.active ]
                                []
                            , text ""
                            ]
                        ]

                Nothing ->
                    div [] [ text "Mission missing!" ]


renderCurriculum : Model -> Html msg
renderCurriculum model =
    let
        renderHeader gradeLevels =
            tr []
                (th [] []
                    :: (gradeLevels
                            |> List.map
                                (\gradeLevel ->
                                    th [] [ text gradeLevel.code ]
                                )
                       )
                )

        renderBody domains gradeLevels missions =
            domains
                |> List.map
                    (\domain ->
                        tr []
                            (th [] [ text domain.code ]
                                :: (gradeLevels |> List.map (\gradeLevel -> renderCell domain gradeLevel missions))
                            )
                    )

        renderCell : Domain -> GradeLevel -> List Mission -> Html msg
        renderCell domain gradeLevel missions =
            let
                match mission =
                    mission.domainId == domain.id && mission.gradeLevelId == gradeLevel.id
            in
            td []
                (missions
                    |> List.filter match
                    |> List.map renderMissionCell
                )

        renderMissionCell : Mission -> Html msg
        renderMissionCell mission =
            Routing.link (MissionRoute mission.id)
                []
                [ text
                    (String.fromInt mission.activeQuestCount
                        ++ "/"
                        ++ String.fromInt mission.inactiveQuestCount
                    )
                ]
    in
    case ( model.missions, model.gradeLevels, model.domains ) of
        ( RemoteData.Success missions, RemoteData.Success gradeLevels, RemoteData.Success domains ) ->
            table [] (renderHeader gradeLevels :: renderBody domains gradeLevels missions)

        _ ->
            -- TODO: handle error vs loading
            text "Data missing!"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = LinkClicked
        }
