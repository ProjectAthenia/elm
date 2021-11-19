module Utilities.ModelHelpers exposing (..)

import Json.Decode as JsonDecode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode

type alias BaseRecord =
    { id: Maybe Int
    }

type alias GenericModel a =
    { a | id: Maybe Int }


-- Takes a list of models and converts them to a list of ids
pluckIds : List (GenericModel a) -> List (Int)
pluckIds models =
    List.filterMap (\model -> model.id) models


encodeIds : List (GenericModel a) -> Encode.Value
encodeIds models =
    Encode.list Encode.int
        <| pluckIds models


toKeyValuePairs : List (GenericModel a) -> List (Int, GenericModel a)
toKeyValuePairs models =
    List.filterMap keyValuePair models


keyValuePair : GenericModel a -> Maybe (Int, GenericModel a)
keyValuePair model =
    case model.id of
        Just id ->
            Just (id, model)
        Nothing ->
            Nothing


pullFromKeyValueById : Int -> List (Int, GenericModel a) -> Maybe (GenericModel a)
pullFromKeyValueById id models =
    List.head (List.filterMap (checkIfKeyValuePairMatchesID id) models)


checkIfKeyValuePairMatchesID : Int -> (Int, GenericModel a) -> Maybe (GenericModel a)
checkIfKeyValuePairMatchesID id (key, value) =
    if id == key then
        Just value
    else
        Nothing


baseRecordDecoder: Decoder BaseRecord
baseRecordDecoder =
    JsonDecode.succeed BaseRecord
        |> optional "id" (maybe int) Nothing


stringFloatDecoder : Decoder Float
stringFloatDecoder =
  (JsonDecode.string)
      |> JsonDecode.andThen (\val ->
          case String.toFloat val of
              Just f -> JsonDecode.succeed f
              _ -> JsonDecode.fail "Error transforming string encoded float to float"
          )


floatDecoder : Decoder Float
floatDecoder =
    JsonDecode.oneOf
        [ JsonDecode.float
        , stringFloatDecoder
        ]


tinyIntDecoder: Decoder Bool
tinyIntDecoder =
    JsonDecode.andThen (\value -> JsonDecode.succeed <| value > 0) JsonDecode.int
