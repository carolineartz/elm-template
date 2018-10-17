module Mission exposing (Mission, MissionId(..), decoder, fetchAll, unwrapId)

import Domain exposing (DomainId(..))
import GradeLevel exposing (GradeLevelId(..))
import Http
import HttpBuilder exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


type MissionId
    = MissionId Int


type alias Mission =
    { id : MissionId
    , gradeLevelId : GradeLevelId
    , domainId : DomainId
    , activeQuestCount : Int
    , inactiveQuestCount : Int
    , helpText : Maybe String
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
        |> required "help_text" (Decode.nullable Decode.string)
        |> required "active" Decode.bool


unwrapId : MissionId -> Int
unwrapId (MissionId id) =
    id


fetchAll =
    HttpBuilder.get "http://localhost:3000/missions"
        |> HttpBuilder.withExpectJson (Decode.list decoder)
