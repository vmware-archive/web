port module SpaceTopBar exposing (Model, Msg(..), fetchUser, init, update, urlUpdate, view)

import SpaceRoutes
import Concourse
import Concourse.User
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, style)
import Http
import Navigation exposing (Location)
import QueryString
import String
import Task


type alias Model =
    { route : SpaceRoutes.ConcourseRoute
    , userState : UserState
    , userMenuVisible : Bool
    }


type UserState
    = UserStateLoggedIn Concourse.User
    | UserStateLoggedOut
    | UserStateUnknown


type Msg
    = Noop
    | UserFetched (Result Http.Error Concourse.User)
    | NavTo String


init : SpaceRoutes.ConcourseRoute -> ( Model, Cmd Msg )
init route =
    let
        pid =
            extractPidFromRoute route.logical
    in
        ( { route = route
          , userState = UserStateUnknown
          , userMenuVisible = False
          }
        , fetchUser
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        UserFetched (Ok user) ->
            ( { model | userState = UserStateLoggedIn user }
            , Cmd.none
            )

        UserFetched (Err _) ->
            ( { model | userState = UserStateLoggedOut }
            , Cmd.none
            )

        NavTo url ->
            ( model, Navigation.newUrl url )


extractPidFromRoute : SpaceRoutes.Route -> Maybe Concourse.PipelineIdentifier
extractPidFromRoute route =
    case route of
        SpaceRoutes.SpacePipeline teamName pipelineName ->
            Just { teamName = teamName, pipelineName = pipelineName }

        SpaceRoutes.SpaceBuild teamName pipelineName _ _ _ ->
            Just { teamName = teamName, pipelineName = pipelineName }

        SpaceRoutes.SpaceHome ->
            Nothing


urlUpdate : SpaceRoutes.ConcourseRoute -> Model -> ( Model, Cmd Msg )
urlUpdate route model =
    ( { model
        | route = route
      }
    , fetchUser
    )


pidToUrl : Maybe Concourse.PipelineIdentifier -> SpaceRoutes.ConcourseRoute -> String
pidToUrl pid { queries } =
    case pid of
        Just { teamName, pipelineName } ->
            String.join ""
                [ String.join "/"
                    [ "/teams"
                    , teamName
                    , "pipelines"
                    , pipelineName
                    ]
                , QueryString.render queries
                ]

        Nothing ->
            ""


view : Model -> Html Msg
view model =
    Html.nav
        [ classList
            [ ( "top-bar", True )
            , ( "test", True )
            ]
        ]
        [ Html.ul [ class "nav-right" ]
            [ Html.li [ class "nav-item" ]
                [ viewUserState model.userState model.userMenuVisible
                ]
            ]
        ]


isPaused : Maybe Concourse.Pipeline -> Bool
isPaused =
    Maybe.withDefault False << Maybe.map .paused


viewUserState : UserState -> Bool -> Html Msg
viewUserState userState userMenuVisible =
    case userState of
        UserStateUnknown ->
            Html.text ""

        UserStateLoggedOut ->
            Html.text ""

        UserStateLoggedIn user ->
            Html.div [ class "user-info" ]
                [ Html.div [ class "user-id" ]
                    [ Html.i [ class "fa fa-user" ] []
                    , Html.text " "
                    , Html.text <| userDisplayName user
                    , Html.text " "
                    , Html.i [ class "fa fa-caret-down" ] []
                    ]
                , Html.div [ classList [ ( "user-menu", True ), ( "hidden", not userMenuVisible ) ] ]
                    [ Html.a
                        [ Html.Attributes.attribute "aria-label" "Log Out"
                        ]
                        [ Html.text "logout"
                        ]
                    ]
                ]


userDisplayName : Concourse.User -> String
userDisplayName user =
    Maybe.withDefault user.id <|
        List.head <|
            List.filter (not << String.isEmpty) [ user.userName, user.name, user.email ]


fetchUser : Cmd Msg
fetchUser =
    Task.attempt UserFetched Concourse.User.fetchUser
