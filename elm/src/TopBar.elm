port module TopBar exposing (Model, Msg(..), UserState(..), fetchUser, update, userDisplayName)

import Concourse
import Concourse.Pipeline
import Concourse.User
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, style)
import Html.Events exposing (onClick)
import Http
import LoginRedirect
import Navigation exposing (Location)
import Pipeline
import Routes
import StrictEvents exposing (onLeftClickOrShiftLeftClick)
import Task
import Time


type alias Model =
    { route : Routes.ConcourseRoute
    , userState : UserState
    , userMenuVisible : Bool
    }


type UserState
    = UserStateLoggedIn Concourse.User
    | UserStateLoggedOut
    | UserStateUnknown


type Msg
    = Noop
    | FetchUser Time.Time
    | UserFetched (Result Http.Error Concourse.User)
    | ResetToPipeline String
    | ToggleUserMenu


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        FetchUser _ ->
            ( model, fetchUser )

        UserFetched (Ok user) ->
            ( { model | userState = UserStateLoggedIn user }
            , Cmd.none
            )

        UserFetched (Err _) ->
            ( { model | userState = UserStateLoggedOut }
            , Cmd.none
            )

        ResetToPipeline url ->
            ( model, Cmd.batch [ Navigation.newUrl url, Pipeline.resetPipelineFocus () ] )

        ToggleUserMenu ->
            ( { model | userMenuVisible = not model.userMenuVisible }, Cmd.none )


viewBreadcrumbs : Model -> List (Html Msg)
viewBreadcrumbs model =
    List.intersperse viewBreadcrumbSeparator <|
        case model.route.logical of
            Routes.Pipeline teamName pipelineName ->
                [ viewBreadcrumbPipeline pipelineName model.route.logical ]

            Routes.Job teamName pipelineName jobName ->
                [ viewBreadcrumbPipeline pipelineName <| Routes.Pipeline teamName pipelineName
                , viewBreadcrumbJob jobName
                ]

            Routes.Build teamName pipelineName jobName buildName ->
                [ viewBreadcrumbPipeline pipelineName <| Routes.Pipeline teamName pipelineName
                , viewBreadcrumbJob jobName
                ]

            Routes.Resource teamName pipelineName resourceName ->
                [ viewBreadcrumbPipeline pipelineName <| Routes.Pipeline teamName pipelineName
                , viewBreadcrumbResource resourceName
                ]

            _ ->
                []


viewBreadcrumbSeparator : Html Msg
viewBreadcrumbSeparator =
    Html.li [ class "nav-item" ] [ Html.text "/" ]


viewBreadcrumbPipeline : String -> Routes.Route -> Html Msg
viewBreadcrumbPipeline pipelineName route =
    let
        url =
            Routes.toString route
    in
        Html.li [ class "nav-item" ]
            [ Html.a
                [ StrictEvents.onLeftClick <| ResetToPipeline url
                , href url
                ]
                [ Html.div [ class "breadcrumb-icon breadcrumb-pipeline-icon" ] []
                , Html.text <| decodeName pipelineName
                ]
            ]


viewBreadcrumbJob : String -> Html Msg
viewBreadcrumbJob name =
    Html.li [ class "nav-item" ]
        [ Html.div [ class "breadcrumb-icon breadcrumb-job-icon" ] []
        , Html.text <| decodeName name
        ]


viewBreadcrumbResource : String -> Html Msg
viewBreadcrumbResource name =
    Html.li [ class "nav-item" ]
        [ Html.div [ class "breadcrumb-icon breadcrumb-resource-icon" ] []
        , Html.text <| decodeName name
        ]


decodeName : String -> String
decodeName name =
    Maybe.withDefault name (Http.decodeUri name)


isPaused : Maybe Concourse.Pipeline -> Bool
isPaused =
    Maybe.withDefault False << Maybe.map .paused


userDisplayName : Concourse.User -> String
userDisplayName user =
    Maybe.withDefault user.id <|
        List.head <|
            List.filter (not << String.isEmpty) [ user.userName, user.name, user.email ]


fetchUser : Cmd Msg
fetchUser =
    Task.attempt UserFetched Concourse.User.fetchUser
