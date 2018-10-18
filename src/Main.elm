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
import Mission exposing (Mission)
import MissionId exposing (MissionId)
import RemoteData exposing (WebData)
import Routing exposing (Route(..))
import Task
import Url


type alias Model =
    { domains : WebData (List Domain)
    , gradeLevels : WebData (List GradeLevel)
    , missions : WebData (List Mission)
    , route : Maybe Route
    , key : Nav.Key
    , missionUpdateForm : MissionFormModel
    , getCurrentMission : List Mission -> Route -> Maybe Mission
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | ChangedUrl Url.Url
    | DomainsLoadingComplete (Result Http.Error (List Domain))
    | GradeLevelsLoadingComplete (Result Http.Error (List GradeLevel))
    | MissionsLoadingComplete (Result Http.Error (List Mission))
      -- | MissionLoadComplete (Result Http.Error Mission)
    | SubmitMissionUpdateForm
    | SetMissionUpdateFormHelpText String
    | SetMissionUpdateFormActive Bool



-- initialFetchForRoute : Maybe Route -> Cmd Msg


initialFetchForRoute route =
    case route of
        Just CurriculumRoute ->
            -- Task.map3
            Cmd.batch
                [ Domain.fetchAll |> Http.send DomainsLoadingComplete
                , GradeLevel.fetchAll |> Http.send GradeLevelsLoadingComplete
                , Mission.fetchAll |> HttpBuilder.send MissionsLoadingComplete
                ]

        Just (MissionRoute missionId) ->
            Mission.fetchAll
                -- |> HttpBuilder.send MissionsLoadingComplete
                |> HttpBuilder.toRequest
                |> Http.toTask
                |> Task.andThen (Task.attempt Mission.fromRoute route)

        Nothing ->
            Cmd.none



-- setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
-- setRoute maybeRoute model =
--     let
--         transition toMsg task =
--             { model | pageState = TransitioningFrom (getPage model.pageState) }
--                 => Task.attempt toMsg task
--         errored =
--             pageErrored model
--     in
--         case maybeRoute of
--             Nothing ->
--                 { model | pageState = Loaded NotFound } => Cmd.none
--             Just Route.NewArticle ->
--                 case model.session.user of
--                     Just user ->
--                         { model | pageState = Loaded (Editor Nothing Editor.initNew) } => Cmd.none
--                     Nothing ->
--                         errored Page.NewArticle "You must be signed in to post an article."
-- initEdit : Task  Model


initEditMission missionId =
    Mission.fetch
        |> Http.toTask
        -- |> Task.mapError (\_ -> pageLoadError Page.Other "Article is currently unavailable.")
        |> Task.map
            (\mission ->
                { errors = []
                , id = MissionId.toString mission.id
                , helpText = mission.helpText
                , active = mission.active
                }
            )


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    -- let
    -- fetchTask =
    --     Task.map3
    --         (Domain.fetchAll |> Http.send DomainsLoadingComplete)
    --         |> Http.toTask
    --             (GradeLevel.fetchAll |> Http.send GradeLevelsLoadingComplete)
    --         |> Http.toTask
    --             (Mission.fetchAll |> HttpBuilder.send MissionsLoadingComplete)
    --         |> Http.toTask
    --
    -- blah =
    --     Task.sequence [ fetchTask ]
    -- Cmd.batch
    -- [ Domain.fetchAll |> Http.send DomainsLoadingComplete
    -- , GradeLevel.fetchAll |> Http.send GradeLevelsLoadingComplete
    -- , Mission.fetchAll |> HttpBuilder.send MissionsLoadingComplete
    -- ]
    -- in
    ( { gradeLevels = RemoteData.Loading
      , domains = RemoteData.Loading
      , missions = RemoteData.Loading
      , route = Routing.fromUrl url
      , key = key
      , missionUpdateForm = emptyMissionUpdateForm
      , getCurrentMission = Mission.fromRoute
      }
    , initialFetchForRoute (Routing.fromUrl url)
    )



-- Task.perform


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MissionsLoadingComplete (Err error) ->
            ( { model | missions = RemoteData.Failure error }, Cmd.none )

        MissionsLoadingComplete (Ok missions) ->
            ( { model | missions = RemoteData.Success missions }, Cmd.none )

        DomainsLoadingComplete (Err error) ->
            ( { model | domains = RemoteData.Failure error }, Cmd.none )

        DomainsLoadingComplete (Ok domains) ->
            ( { model | domains = RemoteData.Success domains }, Cmd.none )

        GradeLevelsLoadingComplete (Err error) ->
            ( { model | domains = RemoteData.Failure error }, Cmd.none )

        GradeLevelsLoadingComplete (Ok gradeLevels) ->
            ( { model | gradeLevels = RemoteData.Success gradeLevels }, Cmd.none )

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
                                [ text <| "mission_id: " ++ MissionId.toString aMission.id ]
                            , p []
                                [ text <| "help_text: " ++ aMission.helpText ]
                            , p []
                                [ text <| "active: " ++ Debug.toString aMission.active ]
                            ]
                        , Html.form [ onSubmit SubmitMissionUpdateForm ]
                            [ input [ name "id", type_ "hidden", value <| MissionId.toString aMission.id ]
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
