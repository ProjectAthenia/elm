module Components.CRUD.ModelForm exposing (..)

import Api exposing (Error, Token)
import Api.Group exposing (..)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Browser.Navigation as Navigation
import Components.CRUD.SharedConfiguration as SharedConfiguration
import Components.LoadingIndicator as LoadingIndicator
import Components.Toast as Toast
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit)
import Http as Http exposing (..)
import Json.Decode exposing (..)
import Json.Encode as Encode
import List.Extra as ListExtra
import Utilities.Expands as Expands
import Utilities.ModelHelpers exposing (..)


type Msg dataModel childMsg
    = ModelCreatedResponse (Result Api.Error (GenericModel dataModel))
    | ModelUpdatedResponse (Result Api.Error (GenericModel dataModel))
    | ModelLoadedResponse (Result Api.Error (GenericModel dataModel))
    | ChildMsg childMsg
    | RemoveToast Toast.Model
    | Save


type alias ModelEncoder dataModel
    = GenericModel dataModel -> Encode.Value


type alias ValidateModel dataModel childModel
    = GenericModel dataModel -> childModel -> Result String (GenericModel dataModel)


-- The init function format that must return our form model, and any needed commands
type alias ChildInit childModel childMsg =
    String -> Token -> (childModel, Cmd childMsg)


-- The update function format for any form update calls
type alias ChildUpdate dataModel childModel childMsg =
    Token -> GenericModel dataModel -> childMsg -> childModel -> (childModel, Cmd childMsg)


-- An alias to our form view function that all of our forms must pass in
type alias ChildView dataModel childModel childMsg =
    GenericModel dataModel -> childModel -> Html childMsg


type alias ChildAction dataModel childModel childMsg =
    Token -> GenericModel dataModel -> childModel -> (childModel, Cmd childMsg)


-- An alias to an optional callback that can be set,
-- which will be called after every child update to check to see if a save should be run
type alias ShouldRunSave dataModel childModel =
    GenericModel dataModel -> childModel -> Maybe (GenericModel dataModel)


-- This configuration model is everything that needs to be specifically configured for the form
type alias Configuration dataModel childModel childMsg =
    { createEncoder: ModelEncoder dataModel
    , updateEncoder: ModelEncoder dataModel
    , newModel: GenericModel dataModel
    , validateModel: ValidateModel dataModel childModel
    , childInit: ChildInit childModel childMsg
    , childUpdate: ChildUpdate dataModel childModel childMsg
    , setModel: ChildAction dataModel childModel childMsg
    , modelCreated: ChildAction dataModel childModel childMsg
    , modelUpdated: ChildAction dataModel childModel childMsg
    , shouldRunSave: ShouldRunSave dataModel childModel
    , fields: List (Field childModel childMsg)
    }


type alias Field childModel childMsg =
    Bool -> childModel -> Html childMsg


type alias Model dataModel childModel childMsg =
    { sharedConfiguration: SharedConfiguration.Configuration dataModel
    , configuration: Configuration dataModel childModel childMsg
    , navigationKey: Navigation.Key
    , loading: Bool
    , dataModel: (GenericModel dataModel)
    , childModel: childModel
    , toasts: List Toast.Model
    }


configure: ModelEncoder dataModel -> ModelEncoder dataModel-> GenericModel dataModel -> ValidateModel dataModel childModel
    -> ChildInit childModel childMsg -> ChildUpdate dataModel childModel childMsg
    -> Configuration dataModel childModel childMsg
configure createEncoder updateEncoder newModel validateModel childInit childUpdate =
    { createEncoder = createEncoder
    , updateEncoder = updateEncoder
    , newModel = newModel
    , validateModel = validateModel
    , childInit = childInit
    , childUpdate = childUpdate
    , setModel = noAction
    , modelCreated = noAction
    , modelUpdated = noAction
    , shouldRunSave = \_ _ -> Nothing
    , fields = []
    }


configureSetModelAction: ChildAction dataModel childModel childMsg -> Configuration dataModel childModel childMsg
    -> Configuration dataModel childModel childMsg
configureSetModelAction setModelAction configuration =
    { configuration
        | setModel = setModelAction
    }


configureModelCreatedAction: ChildAction dataModel childModel childMsg -> Configuration dataModel childModel childMsg
    -> Configuration dataModel childModel childMsg
configureModelCreatedAction modelCreated configuration =
    { configuration
        | modelCreated = modelCreated
    }


configureModelUpdatedAction: ChildAction dataModel childModel childMsg -> Configuration dataModel childModel childMsg
    -> Configuration dataModel childModel childMsg
configureModelUpdatedAction modelUpdated configuration =
    { configuration
        | modelUpdated = modelUpdated
    }


configureShouldRunSave: ShouldRunSave dataModel childModel -> Configuration dataModel childModel childMsg
    -> Configuration dataModel childModel childMsg
configureShouldRunSave shouldRunSave configuration =
    { configuration
        | shouldRunSave = shouldRunSave
    }


addFields: Configuration dataModel childModel childMsg -> List (Field childModel childMsg) -> Configuration dataModel childModel childMsg
addFields configuration fields =
    { configuration
        | fields = List.append configuration.fields fields
    }


addField: Configuration dataModel childModel childMsg -> Field childModel childMsg -> Configuration dataModel childModel childMsg
addField configuration field =
    addFields configuration [field]


initialState :  SharedConfiguration.Configuration dataModel -> Configuration dataModel childModel childMsg -> Navigation.Key -> Token -> Maybe Int
    -> (Model dataModel childModel childMsg, Cmd (Msg dataModel childMsg))
initialState sharedConfiguration configuration navigationKey token maybeId =
    let
        (childModel, childCmd) =
            configuration.childInit sharedConfiguration.apiUrl token
    in
    ( { sharedConfiguration = sharedConfiguration
      , configuration = configuration
      , navigationKey = navigationKey
      , loading = maybeId /= Nothing
      , dataModel = configuration.newModel
      , childModel = childModel
      , toasts = []
    }
    , Cmd.batch
        [ case maybeId of
            Just id ->
                getModel token sharedConfiguration.routeGroup.existing sharedConfiguration.expands id sharedConfiguration.decoder
            Nothing ->
                Cmd.none
        , Cmd.map ChildMsg childCmd
        ]
    )


setModel : Token -> Model dataModel childModel childMsg -> (GenericModel dataModel) -> Bool -> (Model dataModel childModel childMsg, Cmd (Msg dataModel childMsg))
setModel token model dataModel loading =
    let
        (childModel, childMsg) =
            model.configuration.setModel token dataModel model.childModel
    in
    ( { model
        | loading = loading
        , dataModel = dataModel
        , childModel = childModel
    }
    , Cmd.map ChildMsg childMsg
    )


update : Token -> Msg dataModel childMsg -> Model dataModel childModel childMsg -> (Model dataModel childModel childMsg, Cmd (Msg dataModel childMsg))
update token msg model =
    case msg of
        ModelCreatedResponse (Ok dataModel) ->
            case dataModel.id of
                Just id ->
                    let
                        (updatedModel, toastCmd) =
                            Toast.appendToast
                                (Toast.createToast Toast.Success RemoveToast "Created Successfully")
                                model

                        (childModel, childMsg) =
                            model.configuration.modelCreated token dataModel model.childModel
                    in
                    ( { updatedModel
                        | childModel = childModel
                    }
                    , Cmd.batch
                        [ toastCmd
                        , Cmd.map ChildMsg childMsg
                        , Navigation.pushUrl model.navigationKey (model.sharedConfiguration.pageUrl ++  "/" ++ (String.fromInt id))
                        ]
                    )

                Nothing ->
                    Toast.appendToast
                        (Toast.createToast Toast.Error RemoveToast "Unknown error creating")
                        { model | loading = False }

        ModelCreatedResponse (Err error) ->
            Toast.appendToast
                (Toast.createToast Toast.Error RemoveToast <| Api.createErrorText "Error Creating" error)
                { model | loading = False }

        ModelUpdatedResponse (Ok dataModel) ->
            let
                (updatedModel, toastCmd) =
                    Toast.appendToast
                        (Toast.createToast Toast.Success RemoveToast "Created Successfully")
                        model

                (childModel, childMsg) =
                    model.configuration.modelUpdated token dataModel model.childModel
            in
            ( { updatedModel
                | childModel = childModel
                , loading = False
            }
            , Cmd.batch
                [ toastCmd
                , Cmd.map ChildMsg childMsg
                ]
            )

        ModelUpdatedResponse (Err error) ->
            Toast.appendToast
                (Toast.createToast Toast.Error RemoveToast <| Api.createErrorText "Error Updating" error)
                { model | loading = False }

        ModelLoadedResponse (Ok dataModel) ->
            setModel token model dataModel False

        ModelLoadedResponse (Err error) ->
            Toast.appendToast
                (Toast.createToast Toast.Error RemoveToast <| Api.createErrorText "Error Loading" error)
                { model | loading = False }

        ChildMsg childMsg ->
            let
                (updatedModel, childCmd) =
                    Tuple.mapBoth (setChildModel model) (Cmd.map ChildMsg)
                        <| model.configuration.childUpdate token model.dataModel childMsg model.childModel
            in
            case model.configuration.shouldRunSave model.dataModel model.childModel of
                Just dataModel ->
                    Tuple.mapSecond (\saveCmd -> Cmd.batch [childCmd, saveCmd] )
                        <| save token updatedModel dataModel

                Nothing ->
                    ( updatedModel
                    , childCmd
                    )

        RemoveToast toast ->
            ( { model
                | toasts = ListExtra.remove toast model.toasts
            }
            , Cmd.none
            )

        Save ->
            case model.configuration.validateModel model.dataModel model.childModel of
                Err errorMessage ->
                    Toast.appendToast
                        (Toast.createToast Toast.Error RemoveToast errorMessage)
                        model

                Ok dataModel ->
                    save token model dataModel


save: Token -> Model dataModel childModel childMsg -> GenericModel dataModel ->  (Model dataModel childModel childMsg, Cmd (Msg dataModel childMsg))
save token model dataModel =
    ( { model
        | loading = True
        , dataModel = dataModel
    }
    , case dataModel.id of
        Just id ->
            updateModel token model.sharedConfiguration.routeGroup.existing id model.sharedConfiguration.decoder model.configuration.updateEncoder dataModel
        Nothing ->
            createModel token model.sharedConfiguration.routeGroup.new model.sharedConfiguration.decoder model.configuration.createEncoder dataModel
    )


noAction: ChildAction dataModel childModel childMsg
noAction _ _ childModel =
    ( childModel, Cmd.none )


setChildModel: Model dataModel childModel childMsg -> childModel -> Model dataModel childModel childMsg
setChildModel model childModel =
    { model
        | childModel = childModel
    }


view : String -> Model dataModel childModel childMsg -> List (Html (Msg dataModel childMsg))
view actionName model =
    [ h1 [] [ text (actionName ++ " " ++ model.sharedConfiguration.resourceName) ]
    , Form.form
        [ onSubmit Save, class "model_form" ]
            <| List.concat
                [ (List.map (\field -> Html.map ChildMsg <| field model.loading model.childModel) model.configuration.fields)
                , [submitButton model.loading]
                ]
    , LoadingIndicator.view model.loading
    , Toast.view model.toasts
    ]


submitButton : Bool -> Html msg
submitButton loading =
    Button.button
        [ Button.success
        , Button.large
        , Button.attrs [ disabled loading ]
        ]
        [ text "Save" ]


createModel : Token -> NewEndpoint -> Decoder (GenericModel dataModel) -> ModelEncoder dataModel -> GenericModel dataModel -> Cmd (Msg dataModel childMsg)
createModel token newEndpoint decoder createEncoder model =
    Api.createModel newEndpoint token (Http.jsonBody (createEncoder model)) decoder ModelCreatedResponse


updateModel : Token -> ExistingEndpoint -> Int -> Decoder (GenericModel dataModel) -> ModelEncoder dataModel -> GenericModel dataModel -> Cmd (Msg dataModel childMsg)
updateModel token existingEndpoint id decoder updateEncoder model =
    Api.updateModel (existingEndpoint [] id) token (Http.jsonBody (updateEncoder model)) decoder ModelUpdatedResponse


getModel : Token -> ExistingEndpoint -> List Expands.Expand -> Int -> Decoder (GenericModel dataModel) -> Cmd (Msg dataModel childMsg)
getModel token existingEndpoint expands id decoder =
    Api.get (existingEndpoint (Expands.toQueryParameters expands) id) (Just token) decoder ModelLoadedResponse


mapSubscription: Sub childMsg -> Sub (Msg dataModel childMsg)
mapSubscription sub =
    Sub.map ChildMsg sub
