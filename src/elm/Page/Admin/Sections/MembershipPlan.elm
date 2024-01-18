module Page.Admin.Sections.MembershipPlan exposing (..)

import Api exposing (Token)
import Browser.Navigation as Navigation
import Components.CRUD.ModelForm.Input as Input
import Components.CRUD.ModelForm.NumberField as NumberField
import Components.CRUD.ModelForm.TextField as TextField
import Components.CRUD.ModelForm.SelectField as SelectField
import Components.CRUD.ModelForm as ModelForm
import Components.CRUD.ModelList as ModelList
import Components.CRUD.RootController as RootController
import Components.CRUD.SharedConfiguration as SharedConfiguration
import Components.MembershipPlan.RateHistory as RateHistory
import Html exposing (..)
import Models.MembershipPlan.MembershipPlan as MembershipPlan
import Utilities.SearchField exposing (SearchFieldType(..))
import Utilities.ModelHelpers exposing (..)


type alias Model =
    RootController.Model MembershipPlan.Model FormModel FormMsg


type alias Msg =
    RootController.Msg MembershipPlan.Model FormMsg


type alias FormModel =
    { apiUrl: String
    , name: String
    , duration: String
    , current_cost: Float
    , rateHistory: Maybe RateHistory.Model
    }

type FormMsg
    = SetName String
    | SetDuration String
    | SetCurrentCost String
    | RateHistoryMsg RateHistory.Msg


sharedConfiguration: String -> SharedConfiguration.Configuration MembershipPlan.Model
sharedConfiguration apiUrl =
    SharedConfiguration.configure apiUrl "Membership Plan" "membership-plans" (MembershipPlan.routeGroup apiUrl) MembershipPlan.modelDecoder []


nameColumn: ModelList.Column MembershipPlan.Model FormMsg
nameColumn =
     ModelList.column
        "Name"
        (\membershipPlan -> Html.text membershipPlan.name)
        "name"
        Text


durationColumn: ModelList.Column MembershipPlan.Model FormMsg
durationColumn =
     ModelList.column
        "Duration"
        (\membershipPlan -> Html.text membershipPlan.duration)
        "duration"
        (Select [("lifetime", "Lifetime"), ("monthly", "Monthly"), ("yearly", "Yearly")])


currentCostColumn: ModelList.Column MembershipPlan.Model FormMsg
currentCostColumn =
     ModelList.column
        "Current Cost"
        (\membershipPlan -> Html.text (String.fromFloat membershipPlan.current_cost))
        ""
        None


indexColumns: List (ModelList.Column MembershipPlan.Model FormMsg)
indexColumns =
    [ nameColumn
    , durationColumn
    , currentCostColumn
    ]


indexConfiguration: ModelList.Configuration MembershipPlan.Model FormMsg
indexConfiguration =
    ModelList.configure [] indexColumns


validateForm: MembershipPlan.Model -> FormModel -> Result String MembershipPlan.Model
validateForm model form =
    if String.length form.name < 1 then
        Err "Please enter the membership plan name"
    else if String.length form.duration < 1 then
        Err "Please select a duration"
    else
        Ok { model
            | name = form.name
            , duration = form.duration
            , current_cost = form.current_cost
           }


initForm: String -> Token -> Navigation.Key -> (FormModel, Cmd FormMsg)
initForm apiUrl token navKey =
    ( { apiUrl = apiUrl
      , name = ""
      , duration = ""
      , current_cost = 0.0
      , rateHistory = Nothing
    }
    , Cmd.none
    )


updateForm: Token -> MembershipPlan.Model -> FormMsg -> FormModel -> (FormModel, Cmd FormMsg)
updateForm token dataModel msg model =
    case msg of
        SetName name ->
            ( { model
                | name = name
            }
            , Cmd.none
            )

        SetDuration duration ->
            ( { model
                | duration = duration
            }
            , Cmd.none
            )

        SetCurrentCost currentCost ->
            ( case String.toFloat currentCost of
                Just cost ->
                    { model
                        | current_cost = cost
                    }

                Nothing ->
                    model
            , Cmd.none
            )

        RateHistoryMsg subMsg ->
            case model.rateHistory of
                Just rateHistory ->
                    Tuple.mapBoth (\updated -> {model | rateHistory = Just updated}) (Cmd.map RateHistoryMsg)
                        <| RateHistory.update token subMsg rateHistory

                Nothing ->
                    (model, Cmd.none)


setModel: Token -> GenericModel MembershipPlan.Model -> FormModel -> (FormModel, Cmd FormMsg)
setModel token dataModel model =
    let
        (maybeRateHistory, rateHistoryCmd) =
            case (dataModel.id, model.rateHistory) of
                (Just id, Just rateHistory) ->
                    if id == rateHistory.membershipPlanId then
                        (Just rateHistory, Cmd.none)
                    else
                        Tuple.mapFirst Just
                            <| RateHistory.initialModel model.apiUrl token id

                (Just id, Nothing) ->
                    Tuple.mapFirst Just
                        <| RateHistory.initialModel model.apiUrl token id

                (Nothing, _) ->
                    (Nothing, Cmd.none)
    in
    ( { model
      | name = dataModel.name
      , duration = dataModel.duration
      , current_cost = dataModel.current_cost
      , rateHistory = maybeRateHistory
    }
    , Cmd.map RateHistoryMsg rateHistoryCmd
    )


nameInput: Bool -> FormModel -> Html FormMsg
nameInput isLoading model =
    TextField.view (Input.configure True "Name" "name") model.name SetName isLoading


durationOptions: List (String, String)
durationOptions =
    [ ("lifetime", "Lifetime")
    , ("yearly", "Yearly")
    , ("monthly", "Monthly")
    ]


durationSelect: Bool -> FormModel -> Html FormMsg
durationSelect isLoading model =
    SelectField.view (SelectField.configure durationOptions "duration" "Duration" True) model.duration SetDuration isLoading


currentCostInput: Bool -> FormModel -> Html FormMsg
currentCostInput isLoading model =
    NumberField.view (Input.configure True "Current Cost" "current_cost") model.current_cost SetCurrentCost isLoading


rateHistoryView: Bool -> FormModel -> Html FormMsg
rateHistoryView _ model =
    case model.rateHistory of
        Just rateHistory ->
            Html.map RateHistoryMsg
                <| RateHistory.view rateHistory

        Nothing ->
            text ""


formConfiguration: ModelForm.Configuration MembershipPlan.Model FormModel FormMsg
formConfiguration =
    let
        baseConfig = ModelForm.configure MembershipPlan.toJson MembershipPlan.toJson MembershipPlan.newModel validateForm initForm updateForm
    in
    ModelForm.addFields (ModelForm.configureSetModelAction setModel baseConfig)
        [ nameInput
        , durationSelect
        , currentCostInput
        , rateHistoryView
        ]


configuration: String -> RootController.Configuration MembershipPlan.Model FormModel FormMsg
configuration apiUrl =
    RootController.configure (sharedConfiguration apiUrl) indexConfiguration formConfiguration


initialModel: String -> RootController.Model MembershipPlan.Model FormModel FormMsg
initialModel apiUrl =
    RootController.initialState
        <| configuration apiUrl
