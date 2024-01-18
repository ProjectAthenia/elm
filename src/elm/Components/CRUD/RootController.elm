module Components.CRUD.RootController exposing (..)

import Api exposing (Token)
import Browser.Navigation as Navigation
import Components.CRUD.ModelForm as ModelForm
import Components.CRUD.ModelList as ModelList
import Components.CRUD.SharedConfiguration as SharedConfiguration
import Html exposing (..)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Create
    | Index
    | Update Int


type State dataModel formModel formMsg
    = Inactive
    | IndexActive (ModelList.Model dataModel formMsg)
    | FormActive (ModelForm.Model dataModel formModel formMsg)


type Msg dataModel formMsg
    = FormMsg (ModelForm.Msg dataModel formMsg)
    | IndexMsg (ModelList.Msg dataModel formMsg)


-- The init function format that must return our form model, and any needed commands
type alias FormInit formModel formMsg =
    Token -> Maybe Int -> (formModel, Cmd formMsg)


-- The update function format for any form update calls
type alias FormUpdate formMsg formModel =
    Token -> formMsg -> formModel -> (formModel, Cmd formMsg)


-- An alias to our form view function that all of our forms must pass in
type alias FormView formModel formMsg =
    String -> formModel -> Html formMsg


type alias Configuration dataModel formModel formMsg =
    { sharedConfiguration: SharedConfiguration.Configuration dataModel
    , indexConfiguration: ModelList.Configuration dataModel formMsg
    , formConfiguration: ModelForm.Configuration dataModel formModel formMsg
    }


type alias Model dataModel formModel formMsg =
    { config: Configuration dataModel formModel formMsg
    , currentRoute: Route
    , currentState: State dataModel formModel formMsg
    , indexModel: Maybe (ModelList.Model dataModel formMsg)
    }


-- Routing
crudRoutes: String -> Parser (Route -> parentRoutes) parentRoutes
crudRoutes name =
    Parser.oneOf
        [ Parser.map Index (Parser.s name)
        , Parser.map Create (Parser.s name </> Parser.s "create")
        , Parser.map Update (Parser.s name </> Parser.int)
        ]



-- Transforms a route into a full string
routeToString: String -> Route -> List String
routeToString name route =
    case route of
        Index ->
            [ name ]

        Create ->
            [ name, "create" ]

        Update id ->
            [ name, String.fromInt id ]


configure: SharedConfiguration.Configuration dataModel -> ModelList.Configuration dataModel formMsg
    -> ModelForm.Configuration dataModel formModel formMsg -> Configuration dataModel formModel formMsg
configure sharedConfiguration indexConfiguration formConfiguration =
    { sharedConfiguration = sharedConfiguration
    , indexConfiguration = indexConfiguration
    , formConfiguration = formConfiguration
    }


-- State manipulation below

initialState : Configuration dataModel formModel formMsg -> Model dataModel formModel formMsg
initialState config  =
    { config = config
    , currentRoute = Index
    , currentState = Inactive
    , indexModel = Nothing
    }


replaceFormModel: Model dataModel formModel formMsg -> ModelForm.Model dataModel formModel formMsg -> Model dataModel formModel formMsg
replaceFormModel model formModel =
    { model
        | currentState = FormActive formModel
    }


replaceIndexModel: Model dataModel formModel formMsg -> ModelList.Model dataModel formMsg -> Model dataModel formModel formMsg
replaceIndexModel model indexModel =
    { model
        | indexModel = Just indexModel
        , currentState = IndexActive indexModel
    }


replaceRoute: Model dataModel formModel formMsg -> Route -> Model dataModel formModel formMsg
replaceRoute model currentRoute =
    { model
        | currentRoute = currentRoute
    }


changePage : Navigation.Key -> Token -> Route -> Model dataModel formModel formMsg
    -> (Model dataModel formModel formMsg, Cmd (Msg dataModel formMsg))
changePage navKey token route model =
    let
        modelWithRoute = replaceRoute model route
    in
    case route of
        Index ->
            case modelWithRoute.indexModel of
                -- if we were already at the index we will simply use that state, and reload the page data
                Just indexModel ->
                    ( { modelWithRoute
                        | currentRoute = Index
                        , currentState = IndexActive indexModel
                    }
                    , Cmd.map IndexMsg
                        <| ModelList.reload token indexModel
                    )

                Nothing ->
                    ModelList.initialState modelWithRoute.config.sharedConfiguration modelWithRoute.config.indexConfiguration navKey token
                        |> Tuple.mapFirst (replaceIndexModel modelWithRoute)
                        |> Tuple.mapSecond (Cmd.map IndexMsg)

        Update id ->
            -- TODO check to see if we can maintain the current state from a create form
            ModelForm.initialState model.config.sharedConfiguration model.config.formConfiguration navKey token (Just id)
                |> Tuple.mapFirst (replaceFormModel modelWithRoute)
                |> Tuple.mapSecond (Cmd.map FormMsg)

        Create ->
            ModelForm.initialState model.config.sharedConfiguration model.config.formConfiguration navKey token Nothing
                |> Tuple.mapFirst (replaceFormModel modelWithRoute)
                |> Tuple.mapSecond (Cmd.map FormMsg)


update: Token -> Msg dataModel formMsg -> Model dataModel formModel formMsg
        -> (Model dataModel formModel formMsg, Cmd (Msg dataModel formMsg))
update token msg model =
    case ( msg, model.currentState ) of
        (FormMsg subMsg, FormActive formModel) ->
            ModelForm.update token subMsg formModel
                |> Tuple.mapFirst (replaceFormModel model)
                |> Tuple.mapSecond (Cmd.map FormMsg)

        (IndexMsg subMsg, IndexActive indexModel) ->
            ModelList.update token subMsg indexModel
                |> Tuple.mapFirst (replaceIndexModel model)
                |> Tuple.mapSecond (Cmd.map IndexMsg)

        (_, _) ->
            (model, Cmd.none)


-- View functions

view : Model dataModel formModel formMsg -> List (Html (Msg dataModel formMsg))
view model =
    case (model.currentState, model.currentRoute) of
        (IndexActive indexModel, Index) ->
            List.map (Html.map IndexMsg)
                <| ModelList.view indexModel

        (FormActive formModel, Create) ->
            List.map (Html.map FormMsg)
                <| ModelForm.view "Create" formModel

        (FormActive formModel, Update _) ->
            List.map (Html.map FormMsg)
                <| ModelForm.view "Update" formModel

        _ ->
            [ Html.h1 [] [text "Inactive State"]
            , Html.p [] [text "Tell Bryce, and try refreshing the page"]
            ]


mapSubscription: Sub formMsg -> Sub (Msg dataModel formMsg)
mapSubscription modelSub =
    Sub.map FormMsg
        <| ModelForm.mapSubscription modelSub
