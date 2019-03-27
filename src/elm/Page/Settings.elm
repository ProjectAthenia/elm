module Page.Settings exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api as Api exposing (Token)
import Api.Endpoint as Endpoint
import Components.LoadingIndicator as LoadingIndicator
import Models.Error as Error
import Models.User.User as User
import Route as Route
import Session as Session exposing (Session)
import Utilities.Log as Log
import Viewer as Viewer exposing (Viewer)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Html exposing (Html, div, fieldset, h1, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit)
import Http
import Json.Encode as Encode



-- MODEL


type alias Model =
    { session : Session
    , showLoading : Bool
    , token : Token
    , problems : List Problem
    , status : Status
    }


type alias Form =
    { id : Int
    , name : String
    , email : String
    , password : String
    }


type Status
    = Loading
    | Loaded Form
    | Failed


type Problem
    = InvalidEntry ValidatedField String
    | ServerError Error.Model


init : Session -> Token -> ( Model, Cmd Msg )
init session token =
    ( { session = session
      , showLoading = True
      , token = token
      , problems = []
      , status = Loading
      }
    , Api.get Endpoint.me (Session.token session) User.modelDecoder CompletedFormLoad
    )


{-| A form that has been validated. Only the `edit` function uses this. Its
purpose is to prevent us from forgetting to validate the form before passing
it to `edit`.

This doesn't create any guarantees that the form was actually validated. If
we wanted to do that, we'd need to move the form data into a separate module!

-}
type ValidForm
    = Valid Form



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Settings"
    , content =
        div [ id "settings", class "page" ]
            [ Grid.container []
                [ Grid.row []
                    [ Grid.col [Col.md6, Col.offsetMd3]
                        [ h1 [ class "text-xs-center" ] [ text "Your Settings" ]
                        , div [ ]
                            (List.map viewProblem model.problems)
                        , case model.status of
                            Loaded form ->
                                viewForm model.token form

                            Loading ->
                                text ""

                            Failed ->
                                text "Error loading page."
                        ]
                    ]
                ]
            , LoadingIndicator.view model.showLoading
            ]
    }


viewForm : Token -> Form -> Html Msg
viewForm token form =
    Form.form [ onSubmit (SubmittedForm token form) ]
        [ fieldset [ class "form-group" ]
            [ Input.text
                [ Input.large
                , Input.placeholder "Name"
                , Input.onInput EnteredName
                , Input.value form.name
                , Input.attrs [required True]
                ]
            ]
        , fieldset [ class "form-group" ]
            [ Input.email
                [ Input.large
                , Input.placeholder "Email"
                , Input.onInput EnteredEmail
                , Input.value form.email
                , Input.attrs [required True]
                ]
            ]
        , fieldset [ class "form-group" ]
            [ Input.password
                [ Input.placeholder "New Password"
                , Input.onInput EnteredPassword
                , Input.value form.password
                ]
            ]
        , Button.button
            [ Button.primary
            , Button.large
            ] [ text "Update Settings" ]
        ]



viewProblem : Problem -> Html msg
viewProblem problem =
    case problem of
        InvalidEntry _ str ->
            div [class "error"] [text str]

        ServerError error ->
            Error.view error



-- UPDATE


type Msg
    = SubmittedForm Token Form
    | EnteredName String
    | EnteredEmail String
    | EnteredPassword String
    | CompletedFormLoad (Result Api.Error User.Model)
    | CompletedSave (Result Api.Error User.Model)
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompletedFormLoad (Ok user) ->
            ( { model | showLoading = False, status = Loaded
                { id = user.id
                , name = user.name
                , email = user.email
                , password = ""
                }
            }
            , Cmd.none
            )

        CompletedFormLoad (Err _) ->
            ( { model | showLoading = True, status = Failed }
            , Cmd.none
            )

        SubmittedForm token form ->
            case validate form of
                Ok validForm ->
                    ( { model | showLoading = True, status = Loaded form }
                    , edit token validForm
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredName name ->
            updateForm (\form -> { form | name = name }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedSave (Err error) ->
            case error of
                Api.BadStatus status errorModel ->
                    ( { model | showLoading = False, problems = List.append model.problems [ServerError errorModel] }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        CompletedSave (Ok user) ->
            ( { model | showLoading = False }
            ,  case (Session.token model.session, Session.lastRefresh model.session) of
                (Just token, Just lastRefresh) ->
                    Viewer.store
                        <| Viewer.viewer user token lastRefresh
                _ ->
                    Cmd.none
            )

        GotSession session ->
            case Viewer.maybeToken (Session.viewer session) of
                Just token ->
                    ( { model
                        | session = session
                        , token = token
                    }
                    , Cmd.none
                    )
                Nothing ->
                    ( model
                    , Route.replaceUrl (Session.navKey session) Route.Login
                    )


{-| Helper function for `update`. Updates the form and returns Cmd.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd msg )
updateForm transform model =
    case model.status of
        Loaded form ->
            ( { model | status = Loaded (transform form) }, Cmd.none )

        _ ->
            ( model, Log.error )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- FORM


{-| Marks that we've trimmed the form's fields, so we don't accidentally send
it to the server without having trimmed it!
-}
type TrimmedForm
    = Trimmed Form


{-| When adding a variant here, add it to `fieldsToValidate` too!

NOTE: there are no ImageUrl or Bio variants here, because they aren't validated!

-}
type ValidatedField
    = Username
    | Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Username
    , Email
    , Password
    ]


{-| Trim the form and validate its fields. If there are problems, report them!
-}
validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Username ->
                if String.isEmpty form.name then
                    [ "Name can't be blank." ]

                else
                    []

            Email ->
                if String.isEmpty form.email then
                    [ "Email can't be blank." ]

                else
                    []

            Password ->
                let
                    passwordLength =
                        String.length form.password
                in
                if passwordLength > 0 && passwordLength < Viewer.minPasswordChars then
                    [ "Password must be at least " ++ String.fromInt Viewer.minPasswordChars ++ " characters long." ]

                else
                    []


{-| Don't trim while the user is typing! That would be super annoying.
Instead, trim only on submit.
-}
trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { id = form.id
        , name = String.trim form.name
        , email = String.trim form.email
        , password = String.trim form.password
        }



-- HTTP


{-| This takes a Valid Form as a reminder that it needs to have been validated
first.
-}
edit : Token -> TrimmedForm -> Cmd Msg
edit token (Trimmed form) =
    let
        encodedUser =
            Encode.object
                <| List.concat
                    [ if String.length form.email > 0 then
                        [ ("email", Encode.string form.email) ]
                    else
                        []
                    , if String.length form.password > 0 then
                        [ ("password", Encode.string form.password) ]
                    else
                        []
                    , if String.length form.name > 0 then
                        [ ("name", Encode.string form.name) ]
                    else
                        []
                    ]

        body =
            Http.jsonBody encodedUser
    in
    Api.settings token form.id body CompletedSave