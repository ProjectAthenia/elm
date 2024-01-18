module Components.CRUD.ModelList exposing (..)

import Api exposing (Token)
import Api.Endpoint exposing (Endpoint)
import Models.Page as Page
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Table as Table
import Browser.Navigation as Navigation
import Components.CRUD.SharedConfiguration as SharedConfiguration
import Components.LoadingIndicator as LoadingIndicator
import Http
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import List.Extra as ListExtra
import Modals.Confirmation as ConfirmationModal
import Models.Page as Page
import Models.Status as Status
import Task
import Url.Builder as UrlBuilder exposing (QueryParameter)
import Utilities.Expands as Expands
import Utilities.ModelHelpers as ModelHelpers exposing (GenericModel)
import Utilities.Order as Order
import Utilities.SearchField as SearchField


type alias DataPage dataModel =
    Page.Model (GenericModel dataModel)


type alias DeleteResult =
    Result Http.Error ()


type alias QueryResult dataModel =
    Result Api.Error (DataPage dataModel)


type Msg dataModel subMsg
    = OpenDeleteModal Endpoint
    | LoadPage Int
    | SetSearchFieldValue SearchField.Model String
    | ConfirmDeletion Endpoint
    | CancelDeletion
    | NewDataResponse (QueryResult dataModel)
    | DeleteSuccess DeleteResult
    | CustomButton (String, GenericModel dataModel)
    | CustomColumnMsg subMsg


type alias Column dataModel subMsg =
    { header: String
    , valueCallback: GenericModel dataModel -> Html (Msg dataModel subMsg)
    , searchField: SearchField.Model
    }


type alias CustomColumn dataModel subMsg =
    { header: String
    , row: GenericModel dataModel -> Html subMsg
    }


type alias Configuration dataModel subMsg =
    { orders: List Order.Order
    , columns: List (Column dataModel subMsg)
    , customButtons: List String
    , customColumns: List (CustomColumn dataModel subMsg)
    , createDisabled: Bool
    , deleteEnabled: Bool
    , editEnabled: Bool
    , filters: List SearchField.Model
    , customTitle: Maybe String
    }


type alias Model dataModel subMsg =
    { sharedConfiguration: SharedConfiguration.Configuration dataModel
    , configuration: Configuration dataModel subMsg
    , navigationKey: Navigation.Key
    , loading: Bool
    , columns: List (Column dataModel subMsg)
    , customButtons: List String
    , models: List (Int, GenericModel dataModel)
    , page: Maybe (DataPage dataModel)
    , deleteModal: Maybe (ConfirmationModal.Model (Msg dataModel subMsg))
    , lastLoadedPage: Int
    }


-- All configuration starts here

configure: List Order.Order -> List (Column dataModel subMsg) -> Configuration dataModel subMsg
configure orders columns =
    { orders = orders
    , columns = columns
    , customButtons = []
    , customColumns =[]
    , createDisabled = False
    , deleteEnabled = False
    , editEnabled = True
    , filters = []
    , customTitle = Nothing
    }


disableCreate : Configuration dataModel msg -> Configuration dataModel msg
disableCreate model =
    { model
        | createDisabled = True
    }


disableEdit : Configuration dataModel msg -> Configuration dataModel msg
disableEdit model =
    { model
        | editEnabled = True
    }


enableDelete : Configuration dataModel subMsg -> Configuration dataModel subMsg
enableDelete model =
    { model
        | deleteEnabled = True
    }


setCustomTitle: String -> Configuration dataModel subMsg -> Configuration dataModel subMsg
setCustomTitle customTitle model =
    { model
        | customTitle = Just customTitle
    }


addFilter: String -> String -> Configuration dataModel subMsg -> Configuration dataModel subMsg
addFilter name value model =
    { model
        | filters = SearchField.setValue (SearchField.model name SearchField.Equals) value :: model.filters
    }


addCustomButton: String -> Configuration dataModel subMsg -> Configuration dataModel subMsg
addCustomButton customButton model =
    { model
        | customButtons = List.append model.customButtons [customButton]
    }


addCustomColumn: CustomColumn dataModel subMsg -> Configuration dataModel subMsg -> Configuration dataModel subMsg
addCustomColumn newCustomColumn model =
    { model
        | customColumns = List.append model.customColumns [newCustomColumn]
    }


-- All state manipulations happens here

column : String -> (GenericModel dataModel -> Html (Msg dataModel subMsg)) -> String -> SearchField.SearchFieldType -> Column dataModel subMsg
column header callback searchField searchFieldType =
    { header = header
    , valueCallback = callback
    , searchField = SearchField.model searchField searchFieldType
    }


customColumn: String -> (GenericModel dataModel -> Html subMsg) -> CustomColumn dataModel subMsg
customColumn header row =
    { header = header
    , row = row
    }


initialState : SharedConfiguration.Configuration dataModel -> Configuration dataModel msg -> Navigation.Key -> Token -> (Model dataModel msg, Cmd (Msg dataModel subMsg))
initialState sharedConfiguration configuration navigationKey token =
    let
        model =
            { sharedConfiguration = sharedConfiguration
            , configuration = configuration
            , navigationKey = navigationKey
            , loading = True
            , columns = configuration.columns
            , customButtons = configuration.customButtons
            , models = []
            , page = Nothing
            , deleteModal = Nothing
            , lastLoadedPage = 1
            }
    in
    ( model
    , runQuery token model 1
    )


update : Token -> Msg dataModel subMsg -> Model dataModel msg -> (Model dataModel msg, Cmd (Msg dataModel subMsg))
update token msg model =
    case msg of
        SetSearchFieldValue searchField value ->
            let
                updatedModel =
                    { model | columns = updateSearchField searchField value model.columns }
            in
            ( { updatedModel
                | loading = True
            }
            , runQuery token updatedModel 1
            )

        OpenDeleteModal endpoint ->
            ( { model
                | deleteModal = Just
                    <| ConfirmationModal.showModal
                    <| ConfirmationModal.initialState ("Are you sure you want to delete this " ++ model.sharedConfiguration.resourceName ++ "?") "This could be a real pain in the ass to undo." (ConfirmDeletion endpoint) CancelDeletion
            }
            , Cmd.none
            )

        CancelDeletion ->
            ( { model
                | deleteModal = Nothing
            }
            , Cmd.none
            )

        ConfirmDeletion endpoint ->
            ( { model
                | deleteModal = Nothing
                , loading = True
            }
            , Api.deleteModel endpoint token DeleteSuccess
            )

        LoadPage pageNumber ->
            ( { model
                | loading = True
            }
            , runQuery token model pageNumber
            )

        NewDataResponse (Err _) ->
            -- TODO handle error properly
            ( model, Cmd.none )

        NewDataResponse (Ok dataPage) ->
            ( { model
                | page = Just dataPage
                , models = ModelHelpers.toKeyValuePairs dataPage.data
                , lastLoadedPage = dataPage.current_page
                , loading = False
            }
            , Cmd.none
            )

        DeleteSuccess (Err _) ->
            -- TODO handle error properly
            ( model, Cmd.none )

        DeleteSuccess (Ok _) ->
            ( { model
              | deleteModal = Nothing
              , loading = False
            }
            , case model.page of
                Just currentPage ->
                    runQuery token model currentPage.current_page
                Nothing ->
                    runQuery token model 1
            )

        CustomButton _ ->
            ( model
            , Cmd.none
            )

        CustomColumnMsg _ ->
            ( model
            , Cmd.none
            )


checkIfColumnIsForSearchField: SearchField.Model -> Column dataModel subMsg -> Bool
checkIfColumnIsForSearchField searchField i =
    i.searchField.name == searchField.name


updateColumn: SearchField.Model -> String -> Column dataModel subMsg -> Column dataModel subMsg
updateColumn searchField value i =
    { i
        | searchField = SearchField.setValue searchField value
    }

updateSearchField : SearchField.Model -> String -> List (Column dataModel subMsg) -> List (Column dataModel subMsg)
updateSearchField searchField value columns =
    ListExtra.updateIf (checkIfColumnIsForSearchField searchField) (updateColumn searchField value) columns


-- All view stuff starts here
view : Model dataModel subMsg -> List (Html (Msg dataModel subMsg))
view model =
    List.concat
        [
            [ h2 []
                [ text <|
                    case model.configuration.customTitle of
                        Just title ->
                            title
                        Nothing ->
                            "Manage " ++ model.sharedConfiguration.resourceName ++ "s"
                , if model.configuration.createDisabled == False then
                    Button.linkButton
                        [ Button.primary
                        , Button.attrs
                            [ class "create_btn"
                            , href (model.sharedConfiguration.pageUrl ++ "/create")
                            ]
                        ]
                        [ text ("Add " ++ model.sharedConfiguration.resourceName) ]
                else
                    text ""
                ]
            , Table.table
                { options = [ Table.bordered, Table.striped ]
                , thead = Table.thead []
                    [ Table.tr [] (builderHeader model)
                    ]
                , tbody = Table.tbody []
                    <| List.map (buildRow model) model.models
                }
            , div [ class "pagination" ] (buildPagination model.page)
            ]
            , case model.deleteModal of
                Just modal ->
                    [ ConfirmationModal.view modal
                    ]
                Nothing ->
                    []
            , [ LoadingIndicator.view model.loading ]
        ]


builderHeader : Model dataModel subMsg -> List (Table.Cell (Msg dataModel subMsg))
builderHeader model =
    List.concat
        [ [ Table.th [] [ text "#" ] ]
        , List.map builderHeaderColumnCell model.columns
        , List.map builderHeaderCustomColumnCell model.configuration.customColumns
        , List.map (\i -> Table.th [] []) model.configuration.customButtons
        , [ Table.th [] [] ]
        , if model.configuration.deleteEnabled then [Table.th [] []] else []
        ]

builderHeaderColumnCell : Column a subMsg -> Table.Cell (Msg dataModel subMsg)
builderHeaderColumnCell instance =
    Table.th []
        [ p [] [ text instance.header ]
        , case instance.searchField.type_ of
             SearchField.Number ->
                Input.number
                    [ Input.value instance.searchField.value
                    , Input.onInput (SetSearchFieldValue instance.searchField)
                    ]

             SearchField.Equals ->
                Input.text
                    [ Input.value instance.searchField.value
                    , Input.onInput (SetSearchFieldValue instance.searchField)
                    ]

             SearchField.Text ->
                Input.text
                    [ Input.value instance.searchField.value
                    , Input.onInput (SetSearchFieldValue instance.searchField)
                    ]

             SearchField.None ->
                  text ""

             SearchField.Select options ->
                Select.select
                    [ Select.onChange (SetSearchFieldValue instance.searchField)
                    ]
                    (List.map (\(name, label) -> Select.item [ Attributes.value name ] [ text label ]) options)
        ]


buildRow : Model dataModel subMsg -> (Int, GenericModel dataModel) -> Table.Row (Msg dataModel subMsg)
buildRow model (id, dataModel) =
    Table.tr [] (buildRowCells dataModel id model)


builderHeaderCustomColumnCell : CustomColumn a subMsg -> Table.Cell (Msg dataModel subMsg)
builderHeaderCustomColumnCell instance =
    Table.th [ Table.cellAttr (class "custom-column") ]
        [ p [] [ text instance.header ] ]


buildRowCells : GenericModel dataModel -> Int -> Model dataModel subMsg -> List (Table.Cell (Msg dataModel subMsg) )
buildRowCells dataModel id model =
    List.concat
        [ [ Table.th [] [ text (String.fromInt id) ] ]
        , List.map (buildColumnCells dataModel) model.configuration.columns
        , List.map (buildCustomColumnCells dataModel) model.configuration.customColumns
        , List.concat
            [
                List.map (\customButton ->
                    Table.td [ ]
                        [ Button.button
                            [ Button.light
                            , Button.onClick <| CustomButton (customButton, dataModel)
                            ]
                            [ text customButton ]
                        ]
                ) model.customButtons
                , if model.configuration.editEnabled then
                    [ Table.td [ ]
                        [ Button.linkButton
                            [ Button.info
                            , Button.attrs
                                [ href <| model.sharedConfiguration.pageUrl ++ "/" ++ (String.fromInt id)
                                ]
                            ]
                            [ text "Edit" ]
                        ]
                    ]
                else
                    []
                , if model.configuration.deleteEnabled then
                    [ Table.td [ ]
                        [ Button.button
                            [ Button.danger
                            , Button.onClick (OpenDeleteModal (model.sharedConfiguration.routeGroup.existing [] id))
                            ]
                            [ text "Delete" ]
                        ]
                    ]
                else
                    []
            ]
        ]


buildColumnCells : GenericModel dataModel -> Column dataModel subMsg -> Table.Cell (Msg dataModel subMsg)
buildColumnCells model instance =
    Table.td [] [ instance.valueCallback model ]


viewBooleanFieldCell: String -> String -> (GenericModel dataModel -> Bool) -> GenericModel dataModel -> Html.Html (Msg dataModel subMsg)
viewBooleanFieldCell positive negative valueCallback dataModel =
    p
        [ class
            <| "alert alert-" ++ (if (valueCallback dataModel) then "success" else "danger")
        ]
        [ text (if (valueCallback dataModel) then positive else negative)
        ]


buildCustomColumnCells : GenericModel dataModel -> CustomColumn dataModel subMsg -> Table.Cell (Msg dataModel subMsg)
buildCustomColumnCells model instance =
    Table.td []
        [ Html.map CustomColumnMsg <| instance.row model
        ]

buildPagination : Maybe (DataPage dataModel) -> List (Html (Msg dataModel subMsg))
buildPagination page =
    case page of
        Just currentPage ->
            List.concat
                [ [ case Page.previousPageNumber currentPage of
                    Just pageNumber ->
                        span [ onClick (LoadPage pageNumber) ] [ text "<" ]
                    Nothing ->
                        span [ class "inactive" ] [ text "<" ]
                ]
                , createPageLinks currentPage
                , [ case Page.nextPageNumber currentPage of
                    Just pageNumber ->
                        span [ onClick (LoadPage pageNumber) ] [ text ">" ]
                    Nothing ->
                        span [ class "inactive" ] [ text ">" ]
                ]
                ]

        Nothing ->
            []


createPageLinks : DataPage dataModel -> List (Html (Msg dataModel subMsg))
createPageLinks page =
    List.map (createPageLink page) (getPageRange page)


getPageRange: DataPage dataModel -> List Int
getPageRange page =
    let
        start = if page.current_page > 6 then page.current_page - 5 else 1
        end = if start + 10 > page.last_page then page.last_page else start + 10
    in
    List.range start end


createPageLink: DataPage dataModel -> Int -> Html (Msg dataModel subMsg)
createPageLink currentPage i =
    if currentPage.current_page == i then
        span [ class "inactive" ] [ text (String.fromInt i) ]
    else
        span [ onClick (LoadPage i) ] [ text (String.fromInt i) ]


-- HTTP stuff starts below

pageDecoder: Decoder (GenericModel dataModel) -> Decoder (Page.Model (GenericModel dataModel))
pageDecoder decoder =
    Page.modelDecoder <| Decode.list <| decoder


reload: Token -> Model dataModel msg -> Cmd (Msg dataModel subMsg)
reload token model =
    runQuery token model model.lastLoadedPage


runQuery: Token -> Model dataModel msg -> Int -> Cmd (Msg dataModel subMsg)
runQuery token model currentPage =
    Api.genericQuery (createQuery model currentPage) token (pageDecoder model.sharedConfiguration.decoder) NewDataResponse


createQuery: Model dataModel msg -> Int -> Endpoint
createQuery model currentPage =
    model.sharedConfiguration.routeGroup.index
        <| List.concat
            [ Expands.toQueryParameters model.sharedConfiguration.expands
            , List.filterMap (\filter -> SearchField.buildSearchFieldQuery filter) model.configuration.filters
            , List.filterMap (\field -> SearchField.buildSearchFieldQuery field.searchField) model.columns
            , [ UrlBuilder.int "page" currentPage
              , UrlBuilder.int "limit" 25
              ]
            , Order.toQueryParameters model.configuration.orders
            ]
