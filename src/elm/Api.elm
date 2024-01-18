port module Api exposing
    ( Token
    , Error(..)
    , storageDecoder
    , unwrapToken
    , decodeErrors
    , delete, get, post, put
    , deleteModel, genericQuery, uploadFile, createModel, updateModel
    , login, logout, signUp, refresh
    , settings, me
    , createUserPaymentMethod
    , createUserSubscription, updateUserSubscription
    , getArticle, viewArticles, createArticle
    , viewArticleIterations
    , getMembershipPlans
    , storeCredWith
    , viewerChanges
    , createErrorText
    )

{-| This module is responsible for communicating to the Conduit API.

It exposes an opaque Endpoint type which is guaranteed to point to the correct URL.

-}

import Api.Endpoint as Endpoint exposing (Endpoint)
import Models.Asset as Asset
import Models.Error as Error
import Models.FileUpload as FileUpload
import Models.Payment.PaymentMethod as PaymentMethod
import Models.MembershipPlan.MembershipPlan as MembershipPlan
import Models.MembershipPlan.Subscription as Subscription
import Models.Status as Status
import Models.User.User as User
import Models.Wiki.Article as Article
import Models.Wiki.Iteration as Iteration
import Http exposing (Body, Expect)
import Json.Decode as Decode exposing (Decoder, Value, decodeString, field)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Json.Encode as Encode



-- CRED


{-| The authentication credentials for the Viewer (that is, the currently logged-in user.)

This includes:

  - The cred's Username
  - The cred's authentication token

By design, there is no way to access the token directly as a String.
It can be encoded for persistence, and it can be added to a header
to a HttpBuilder for a request, but that's it.

This token should never be rendered to the end user, and with this API, it
can't be!

-}
type Token
    = Token String


unwrapToken : Token -> String
unwrapToken (Token str) =
    str


authHeader : Token -> Http.Header
authHeader (Token str) =
    Http.header "Authorization" ("Bearer " ++ str)


{-| It's important that this is never exposed!

We epxose `login` and `application` instead, so we can be certain that if anyone
ever has access to a `Cred` value, it came from either the login API endpoint
or was passed in via flags.

-}
tokenDecoder : Decoder Token
tokenDecoder =
    Decode.succeed Token
        |> required "token" Decode.string


lastRefreshDecoder : Decoder Int
lastRefreshDecoder =
    field "last_refresh" Decode.int



-- PERSISTENCE


decode : Decoder (Token -> Int -> viewer) -> Value -> Result Decode.Error viewer
decode decoder value =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue Decode.string value
        |> Result.andThen (\str -> Decode.decodeString (Decode.field "user" (decoderFromCred decoder)) str)


port onStoreChange : (Value -> msg) -> Sub msg


viewerChanges : (Maybe viewer -> msg) -> Decoder (Token -> Int -> viewer) -> Sub msg
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


decodeFromChange : Decoder (Token -> Int -> viewer) -> Value -> Maybe viewer
decodeFromChange viewerDecoder val =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue (storageDecoder viewerDecoder) val
        |> Result.toMaybe


storeCredWith : Token -> Int -> User.Model -> Cmd msg
storeCredWith (Token token) lastRefresh user =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "token", Encode.string token )
                        , ( "last_refresh", Encode.int lastRefresh )
                        , ( "model", User.cacheEncoder user )
                        ]
                  )
                ]
    in
    storeCache (Just json)


logout : Cmd msg
logout =
    storeCache Nothing


port storeCache : Maybe Value -> Cmd msg



-- SERIALIZATION
-- APPLICATION


storageDecoder : Decoder (Token -> Int -> viewer) -> Decoder viewer
storageDecoder viewerDecoder =
    Decode.field "user" (decoderFromCred viewerDecoder)



-- HTTP


-- All http method utilities begin here

get : Endpoint.Endpoint -> Maybe Token -> Decoder a -> (Result Error a -> msg) -> Cmd msg
get url maybeToken decoder toMsg =
    Endpoint.request
        { method = "GET"
        , headers =
            case maybeToken of
                Just cred ->
                    [ authHeader cred ]

                Nothing ->
                    []
        , url = url
        , expect = expectJson toMsg decoder
        , body = Http.emptyBody
        , timeout = Nothing
        }


put : Endpoint.Endpoint -> Token -> Body -> Decoder a -> (Result Error a -> msg) -> Cmd msg
put url token body decoder toMsg =
    Endpoint.request
        { method = "PUT"
        , headers = [ authHeader token ]
        , url = url
        , expect = expectJson toMsg decoder
        , body = body
        , timeout = Nothing
        }


post : Endpoint.Endpoint -> Maybe Token -> Body -> Decoder a -> (Result Error a -> msg) -> Cmd msg
post url maybeToken body decoder toMsg =
    Endpoint.request
        { method = "POST"
        , headers =
            case maybeToken of
                Just token ->
                    [ authHeader token ]

                Nothing ->
                    []
        , url = url
        , expect = expectJson toMsg decoder
        , body = body
        , timeout = Nothing
        }


delete : Endpoint.Endpoint -> Token -> (Result Http.Error () -> msg) -> Cmd msg
delete url token toMsg =
    Endpoint.request
        { method = "DELETE"
        , headers = [ authHeader token ]
        , url = url
        , expect = Http.expectWhatever toMsg
        , body = Http.emptyBody
        , timeout = Nothing
        }


-- All of our generic functions start here

deleteModel : Endpoint -> Token -> (Result Http.Error () -> msg) -> Cmd msg
deleteModel endpoint token toMsg =
    delete endpoint token toMsg


genericQuery : Endpoint -> Token -> Decoder pageModel -> (Result Error pageModel -> msg) -> Cmd msg
genericQuery endpoint token decoder toMsg =
    get endpoint (Just token) decoder toMsg


uploadFile : Endpoint -> Token -> FileUpload.Model -> (Result Error Asset.Model -> msg) -> Cmd msg
uploadFile endpoint token fileUpload toMsg =
    post endpoint (Just token) (Http.jsonBody (FileUpload.toJson fileUpload)) Asset.modelDecoder toMsg


createModel : Endpoint -> Token -> Http.Body -> Decoder dataModel -> (Result Error dataModel -> msg) -> Cmd msg
createModel endpoint token body decoder toMsg =
    post endpoint (Just token) body decoder toMsg


updateModel : Endpoint -> Token -> Http.Body -> Decoder dataModel -> (Result Error dataModel -> msg) -> Cmd msg
updateModel endpoint token body decoder toMsg =
    put endpoint token body decoder toMsg


-- Specific functions used throughout the base app

login : String -> Http.Body -> (Result Error Token -> msg) -> Cmd msg
login apiUrl body toMsg =
    post (Endpoint.login apiUrl) Nothing body tokenDecoder toMsg


signUp : String -> Http.Body -> (Result Error Token -> msg) -> Cmd msg
signUp apiUrl body toMsg =
    post (Endpoint.signUp apiUrl) Nothing body tokenDecoder toMsg


refresh : String -> Token -> (Result Error Token -> msg) -> Cmd msg
refresh apiUrl token toMsg =
    post (Endpoint.refresh apiUrl) (Just token) Http.emptyBody tokenDecoder toMsg


settings : String -> Token -> Int -> Http.Body -> (Result Error User.Model -> msg) -> Cmd msg
settings apiUrl token userId body toMsg =
    put (Endpoint.user apiUrl userId) token body User.modelDecoder toMsg


me : String -> Token -> (Result Error User.Model -> msg) -> Cmd msg
me apiUrl token toMsg =
    get (Endpoint.me apiUrl) (Just token) User.modelDecoder toMsg


createUserPaymentMethod : String -> Token -> Int -> Http.Body -> (Result Error PaymentMethod.Model -> msg) -> Cmd msg
createUserPaymentMethod apiUrl token userId body toMsg =
    post (Endpoint.userPaymentMethods apiUrl userId) (Just token) body PaymentMethod.modelDecoder toMsg


createUserSubscription : String -> Token -> Int -> Http.Body -> (Result Error Subscription.Model -> msg) -> Cmd msg
createUserSubscription apiUrl token userId body toMsg =
    post (Endpoint.entitySubscriptions apiUrl "users" userId 1) (Just token) body Subscription.modelDecoder toMsg


updateUserSubscription : String -> String -> Token -> Int -> Int -> Http.Body -> (Result Error Subscription.Model -> msg) -> Cmd msg
updateUserSubscription apiUrl entityType token userId subscriptionId body toMsg =
    put (Endpoint.entitySubscription apiUrl entityType userId subscriptionId) token body Subscription.modelDecoder toMsg


createArticle : String -> Token -> Article.CreateModel -> (Result Error Article.Model -> msg) -> Cmd msg
createArticle apiUrl token article  toMsg =
    post (Endpoint.baseArticle apiUrl)
        (Just token)
        (Http.jsonBody (Article.toCreateJson article))
        Article.modelDecoder
        toMsg


getArticle : String -> Token -> Int -> (Result Error Article.Model -> msg) -> Cmd msg
getArticle apiUrl token articleId toMsg =
    get (Endpoint.viewArticle apiUrl articleId) (Just token) Article.modelDecoder toMsg


viewArticles : String -> Token -> Int -> (Result Error Article.Page -> msg) -> Cmd msg
viewArticles apiUrl token page toMsg =
    get (Endpoint.viewArticles apiUrl page) (Just token) Article.pageDecoder toMsg


viewArticleIterations : String -> Token -> Int -> Int -> (Result Error Iteration.Page -> msg) -> Cmd msg
viewArticleIterations apiUrl token articleId page toMsg =
    get (Endpoint.viewArticleIterations apiUrl articleId page) (Just token) Iteration.pageDecoder toMsg


getMembershipPlans : String -> Token -> (Result Error MembershipPlan.Page -> msg) -> Cmd msg
getMembershipPlans apiUrl token toMsg =
    get (Endpoint.membershipPlans apiUrl) (Just token) MembershipPlan.pageDecoder toMsg


decoderFromCred : Decoder (Token -> Int -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map3 (\fromCred cred -> fromCred cred)
        decoder
        tokenDecoder
        lastRefreshDecoder



type Error
    = BadStatus Int Error.Model
    | Timeout
    | BadUrl String
    | NetworkError
    | BadBody String


expectJson : (Result Error a -> msg) -> Decoder a -> Expect msg
expectJson toMsg decoder =
  Http.expectStringResponse toMsg <|
    \response ->
      case response of
        Http.BadUrl_ url ->
          Err (BadUrl url)

        Http.Timeout_ ->
          Err Timeout

        Http.NetworkError_ ->
          Err NetworkError

        Http.BadStatus_ metadata body ->
          Err (BadStatus metadata.statusCode (decodeErrors body))

        Http.GoodStatus_ _ body ->
          case decodeString decoder body of
            Ok value ->
              Ok value

            Err err ->
              Err (BadBody (Decode.errorToString err))

{-| Many API endpoints include an "errors" field in their BadStatus responses.
-}
decodeErrors : String -> Error.Model
decodeErrors body =
    body
        |> decodeString Error.decoder
        |> Result.withDefault Error.unknownErrorResponse


createErrorText : String -> Error -> String
createErrorText title error =
    case error of
        BadStatus status model ->
            title ++ "\n" ++ "Status Code - " ++ String.fromInt status ++ "\n" ++ Error.readError model

        Timeout ->
            title ++ "\n The requested timed out, please try again."

        BadUrl _ ->
            title ++ "\n Bad URL."

        NetworkError ->
            title ++ "\n Network Error. Please check your WiFi connection."

        BadBody message ->
            title ++ "\n - " ++ message
