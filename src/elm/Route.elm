module Route exposing
    ( Route(..)
    , fromUrl
    , href
    , replaceUrl
    )

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Page.Admin.Root as Admin
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, int, string)


-- ROUTING


type Route
    = Home
    | Root
    | Login
    | Logout
    | Settings
    | SignUp
    | Articles
    | Article Int
    | Profile Int
    | EditArticle Int
    | Admin Admin.Route


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map Settings (s "settings")
        , Parser.map Profile (s "profile" </> int)
        , Parser.map SignUp (s "sign-up")
        , Parser.map Articles (s "articles")
        , Parser.map Article (s "articles" </> int)
        , Parser.map EditArticle (s "editor" </> int)
        , Parser.map Admin Admin.routes
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url



-- INTERNAL


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Root ->
                    []

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                SignUp ->
                    [ "sign-up" ]

                Settings ->
                    [ "settings" ]

                Articles ->
                    [ "articles" ]

                Article articleId ->
                    [ "articles", String.fromInt articleId ]

                Profile userId ->
                    [ "profiles", String.fromInt userId ]

                EditArticle articleId ->
                    [ "editor", String.fromInt articleId ]

                Admin subRoute ->
                    Admin.routeToString subRoute
    in
    "/" ++ String.join "/" pieces
