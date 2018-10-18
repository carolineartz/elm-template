module Mission exposing (Mission, decoder, fetch, fetchAll, find, fromRoute)

import Domain exposing (DomainId(..))
import GradeLevel exposing (GradeLevelId(..))
import Http
import HttpBuilder exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List.Extra as ListExtra
import MissionId exposing (MissionId(..))
import Routing exposing (Route(..))


type alias Mission =
    { id : MissionId
    , gradeLevelId : GradeLevelId
    , domainId : DomainId
    , activeQuestCount : Int
    , inactiveQuestCount : Int
    , helpText : String
    , active : Bool
    }


decoder : Decoder Mission
decoder =
    Decode.succeed Mission
        |> required "id" (Decode.map MissionId Decode.int)
        |> required "grade_level_id" (Decode.map GradeLevelId Decode.int)
        |> required "domain_id" (Decode.map DomainId Decode.int)
        |> required "active_quest_count" Decode.int
        |> required "inactive_quest_count" Decode.int
        |> optional "help_text" Decode.string ""
        |> required "active" Decode.bool


find : MissionId -> List Mission -> Maybe Mission
find missionId missions =
    missions
        |> ListExtra.find (\m -> m.id == missionId)



-- fromRoute : List Mission -> Route -> Maybe Mission
-- fromRoute missions route =
--     case route of
--         MissionRoute missionId ->
--             find missionId missions
--         _ ->
--             Nothing


fromRoute : Route -> List Mission -> Maybe Mission
fromRoute route missions =
    case route of
        MissionRoute missionId ->
            find missionId missions

        _ ->
            Nothing


fetchAll =
    HttpBuilder.get "http://localhost:3000/missions"
        |> HttpBuilder.withExpectJson (Decode.list decoder)


fetch id =
    HttpBuilder.get ("http://localhost:3000/missions?id=" ++ MissionId.toString id)
        |> HttpBuilder.withExpectJson decoder
