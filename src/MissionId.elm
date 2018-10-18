module MissionId exposing (MissionId(..), decoder, encode, toString, urlParser)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Url.Parser as Parser exposing (Parser)



-- TYPES


type MissionId
    = MissionId Int



-- CREATE


urlParser : Parser (MissionId -> a) a
urlParser =
    Parser.map MissionId Parser.int


decoder : Decoder MissionId
decoder =
    Decode.map MissionId Decode.int


encode : MissionId -> Value
encode (MissionId missionId) =
    Encode.int missionId



-- TRANSFORM


toString : MissionId -> String
toString (MissionId int) =
    String.fromInt int
