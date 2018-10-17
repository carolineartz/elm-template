module Main exposing (Model)

import Browser
import Browser.Navigation as Nav
import Debug
import Domain exposing (Domain)
import GradeLevel exposing (GradeLevel)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    , missionUpdateForm : MissionFormModel
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | ChangedUrl Url.Url
    | DomainsLoadingComplete (Result Http.Error (List Domain))
    | GradeLevelsLoadingComplete (Result Http.Error (List GradeLevel))
    | MissionsLoadingComplete (Result Http.Error (List Mission))
    | SubmitMissionUpdateForm
    | SetMissionUpdateFormHelpText String
    | SetMissionUpdateFormActive Bool


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        fetchTask =
            Task.map3
                (Domain.fetchAll |> Http.send DomainsLoadingComplete)
                    |> Http.toTask
                (GradeLevel.fetchAll |> Http.send GradeLevelsLoadingComplete)
                    |> Http.toTask
                (Mission.fetchAll |> HttpBuilder.send MissionsLoadingComplete)
                    |> Http.toTask

        blah =
            Task.sequence []
            -- Cmd.batch
            -- [ Domain.fetchAll |> Http.send DomainsLoadingComplete
            -- , GradeLevel.fetchAll |> Http.send GradeLevelsLoadingComplete
            -- , Mission.fetchAll |> HttpBuilder.send MissionsLoadingComplete
            -- ]
        )
    in
        ( { gradeLevels = RemoteData.NotAsked
          , domains = RemoteData.NotAsked
          , missions = RemoteData.NotAsked
          , route = Routing.fromUrl url
          , key = key
          , missionUpdateForm = emptyMissionUpdateForm
          }
        ,

Task.perform

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

        SubmitMissionUpdateForm ->
            ( model, Cmd.none )

        SetMissionUpdateFormHelpText _ ->
            ( model, Cmd.none )

        SetMissionUpdateFormActive isActive ->
            let
                form =
                    model.missionUpdateForm

                updatedForm =
                    { form | active = isActive }
            in
            ( { model | missionUpdateForm = updatedForm }, Cmd.none )



-- ( { model | active = thing }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Test App"
    , body = renderRoute model
    }



-- Let's discuss this
-- It time, provide an example


renderRoute : Model -> List (Html Msg)
renderRoute model =
    case model.route of
        Just CurriculumRoute ->
            [ renderCurriculum model
            ]

        Just (MissionRoute missionId) ->
            [ renderMission model missionId ]

        Nothing ->
            [ h2 [] [ text "Error!, no route found" ] ]



--type alias MissionFormModel =
--    { id : String
--    , helpText : String
--    , active : Bool
--    , errors : []
--    }


type alias MissionFormModel =
    { id : String
    , helpText : String
    , active : Bool
    , errors : List String
    }


emptyMissionUpdateForm =
    { id = ""
    , helpText = ""
    , active = False
    , errors = []
    }


renderMission : Model -> MissionId -> Html Msg
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
                        [ div []
                            [ h1 []
                                [ text "Mission" ]
                            , p []
                                [ text <| "mission_id: " ++ Mission.idToString aMission.id ]
                            , p []
                                [ text <| "help_text: " ++ aMission.helpText ]
                            , p []
                                [ text <| "active: " ++ Debug.toString aMission.active ]
                            ]
                        , Html.form [ onSubmit SubmitMissionUpdateForm ]
                            [ input [ name "id", type_ "hidden", value <| Mission.idToString aMission.id ]
                                []
                            , textarea [ name "help_text", onInput SetMissionUpdateFormHelpText ]
                                [ text aMission.helpText ]
                            , input [ name "active", type_ "checkbox", value "true", checked aMission.active, onCheck SetMissionUpdateFormActive ]
                                []
                            , button [] [ text "submit" ]
                            ]
                        ]

                Nothing ->
                    div [] [ text "Mission missing!" ]


renderCurriculum : Model -> Html Msg
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

        renderCell : Domain -> GradeLevel -> List Mission -> Html Msg
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
