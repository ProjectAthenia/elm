module Components.CRUD.ModelForm.SelectField exposing (..)

import Bootstrap.Form.Select as Select
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)


type alias Model =
    { options: List (String, String)
    , name: String
    , label: String
    , required: Bool
    }


configure: List (String, String) -> String -> String -> Bool -> Model
configure options name label required =
    { options = options
    , name = name
    , label = label
    , required = required
    }


view: Model -> String -> (String -> msg) -> Bool -> Html msg
view model value msg isDisabled =
    div [ class "field" ]
        [ label []
            [ text model.label
            , Select.select
                [ Select.onChange msg
                , Select.disabled isDisabled
                ]
                ( [ defaultOption (String.length value == 0) ] ++ List.map (viewOption value) model.options)
            ]
        ]


defaultOption: Bool -> Select.Item msg
defaultOption selected =
    Select.item [ Attributes.disabled True, Attributes.selected selected ] [ text "Select An Option" ]


viewOption: String -> (String, String) -> Select.Item msg
viewOption value (name, label) =
    Select.item [ Attributes.value name, Attributes.selected (value == name) ] [ text label ]
