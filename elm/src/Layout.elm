port module Layout exposing (Flags, Model, Msg, init, locationMsg, subscriptions, update, view)

import Concourse
import Concourse.Pipeline
import Concourse.User
import Favicon
import Html exposing (Html)
import Html.Attributes as Attributes exposing (attribute, class, classList, disabled, href, id, style)
import Html.Events exposing (onClick)
import Http
import LoginRedirect
import Navigation
import Routes
import SubPage
import StrictEvents exposing (onLeftClickOrShiftLeftClick)
import Task exposing (Task)
import TopBar
import Time


port newUrl : (String -> msg) -> Sub msg


type alias Flags =
    { turbulenceImgSrc : String
    , notFoundImgSrc : String
    , csrfToken : String
    }


type alias NavIndex =
    Int


anyNavIndex : NavIndex
anyNavIndex =
    -1


port saveToken : String -> Cmd msg


port tokenReceived : (Maybe String -> msg) -> Sub msg


port loadToken : () -> Cmd msg


type alias Model =
    { navIndex : NavIndex
    , subModel : SubPage.Model
    , topModel : TopBar.Model
    , topBarType : TopBarType
    , turbulenceImgSrc : String
    , notFoundImgSrc : String
    , csrfToken : String
    , route : Routes.ConcourseRoute
    , pipeline : Maybe Concourse.Pipeline
    }


type TopBarType
    = Dashboard
    | Normal


type Msg
    = Noop
    | RouteChanged Routes.ConcourseRoute
    | SubMsg NavIndex SubPage.Msg
    | TopMsg NavIndex TopBar.Msg
    | NewUrl String
    | ModifyUrl String
    | SaveToken String
    | LoadToken
    | LogIn
    | LogOut
    | LoggedOut (Result Http.Error ())
    | TokenReceived (Maybe String)
    | FetchPipeline Concourse.PipelineIdentifier
    | PipelineFetched (Result Http.Error Concourse.Pipeline)


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        route =
            Routes.parsePath location

        topBarType =
            case route.logical of
                Routes.Dashboard ->
                    Dashboard

                Routes.DashboardHd ->
                    Dashboard

                _ ->
                    Normal

        ( subModel, subCmd ) =
            SubPage.init { turbulencePath = flags.turbulenceImgSrc, csrfToken = flags.csrfToken } route

        ( topModel, topCmd ) =
            let
                pid =
                    extractPidFromRoute route.logical
            in
                ( { route = route
                  , userState = TopBar.UserStateUnknown
                  , userMenuVisible = False
                  }
                , case pid of
                    Nothing ->
                        Task.attempt (TopMsg TopBar.UserFetched) Concourse.User.fetchUser

                    Just pid ->
                        Cmd.batch
                            [ Task.attempt PipelineFetched <|
                                Concourse.Pipeline.fetchPipeline pid
                            , Task.attempt (TopMsg TopBar.UserFetched) Concourse.User.fetchUser
                            ]
                )

        navIndex =
            1

        model =
            { navIndex = navIndex
            , subModel = subModel
            , topModel = topModel
            , topBarType = topBarType
            , turbulenceImgSrc = flags.turbulenceImgSrc
            , notFoundImgSrc = flags.notFoundImgSrc
            , route = route
            , csrfToken = flags.csrfToken
            , pipeline = Nothing
            }

        handleTokenCmd =
            -- We've refreshed on the page and we're not
            -- getting it from query params
            if flags.csrfToken == "" then
                loadToken ()
            else
                saveToken flags.csrfToken

        stripCSRFTokenParamCmd =
            if flags.csrfToken == "" then
                Cmd.none
            else
                Navigation.modifyUrl (Routes.customToString route)
    in
        ( model
        , Cmd.batch
            [ handleTokenCmd
            , stripCSRFTokenParamCmd
            , Cmd.map (SubMsg navIndex) subCmd
            , Cmd.map (TopMsg navIndex) topCmd
            ]
        )


extractPidFromRoute : Routes.Route -> Maybe Concourse.PipelineIdentifier
extractPidFromRoute route =
    case route of
        Routes.Build teamName pipelineName jobName buildName ->
            Just { teamName = teamName, pipelineName = pipelineName }

        Routes.Job teamName pipelineName jobName ->
            Just { teamName = teamName, pipelineName = pipelineName }

        Routes.Resource teamName pipelineName resourceName ->
            Just { teamName = teamName, pipelineName = pipelineName }

        Routes.OneOffBuild buildId ->
            Nothing

        Routes.Pipeline teamName pipelineName ->
            Just { teamName = teamName, pipelineName = pipelineName }

        Routes.Dashboard ->
            Nothing

        Routes.DashboardHd ->
            Nothing


locationMsg : Navigation.Location -> Msg
locationMsg =
    RouteChanged << Routes.parsePath


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            ( model, Navigation.newUrl url )

        ModifyUrl url ->
            ( model, Navigation.modifyUrl url )

        RouteChanged route ->
            urlUpdate route model

        SaveToken tokenValue ->
            ( model, saveToken tokenValue )

        LoadToken ->
            ( model, loadToken () )

        TokenReceived Nothing ->
            ( model, Cmd.none )

        TokenReceived (Just tokenValue) ->
            let
                ( newSubModel, subCmd ) =
                    SubPage.update model.turbulenceImgSrc model.notFoundImgSrc tokenValue (SubPage.NewCSRFToken tokenValue) model.subModel
            in
                ( { model
                    | csrfToken = tokenValue
                    , subModel = newSubModel
                  }
                , Cmd.batch
                    [ Cmd.map (SubMsg anyNavIndex) subCmd
                    ]
                )

        -- otherwise, pass down
        SubMsg navIndex m ->
            if validNavIndex model.navIndex navIndex then
                let
                    ( subModel, subCmd ) =
                        SubPage.update model.turbulenceImgSrc model.notFoundImgSrc model.csrfToken m model.subModel
                in
                    ( { model | subModel = subModel }, Cmd.map (SubMsg navIndex) subCmd )
            else
                ( model, Cmd.none )

        LogOut ->
            ( model, logOut )

        LoggedOut (Ok _) ->
            let
                topModel =
                    model.topModel
            in
                ( { model
                    | topModel =
                        { topModel
                            | userState = TopBar.UserStateLoggedOut
                            , pipeline = Nothing
                        }
                  }
                , Navigation.newUrl "/"
                )

        LoggedOut (Err err) ->
            flip always (Debug.log "failed to log out" err) <|
                ( model, Cmd.none )

        LogIn ->
            let
                topModel =
                    model.topModel
            in
                ( { model | topModel = { topModel | pipeline = Nothing } }
                , LoginRedirect.requestLoginRedirect ""
                )

        FetchPipeline pid ->
            ( model
            , Task.attempt PipelineFetched <|
                Concourse.Pipeline.fetchPipeline pid
            )

        PipelineFetched (Err err) ->
            case err of
                Http.BadStatus { status } ->
                    if status.code == 401 then
                        ( model, LoginRedirect.requestLoginRedirect "" )
                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PipelineFetched (Ok pipeline) ->
            let
                topModel =
                    model.topModel
            in
                ( { model | topModel = { topModel | pipeline = Just pipeline } }
                , Cmd.none
                )

        TopMsg navIndex m ->
            if validNavIndex model.navIndex navIndex then
                let
                    ( topModel, topCmd ) =
                        TopBar.update m model.topModel
                in
                    ( { model | topModel = topModel }, Cmd.map (TopMsg navIndex) topCmd )
            else
                ( model, Cmd.none )

        Noop ->
            ( model, Cmd.none )


logOut : Cmd Msg
logOut =
    Task.attempt LoggedOut Concourse.User.logOut


validNavIndex : NavIndex -> NavIndex -> Bool
validNavIndex modelNavIndex navIndex =
    if navIndex == anyNavIndex then
        True
    else
        navIndex == modelNavIndex


urlUpdate : Routes.ConcourseRoute -> Model -> ( Model, Cmd Msg )
urlUpdate route model =
    let
        navIndex =
            if route == model.route then
                model.navIndex
            else
                model.navIndex + 1

        ( newSubmodel, cmd ) =
            if route == model.route then
                ( model.subModel, Cmd.none )
            else if routeMatchesModel route model then
                SubPage.urlUpdate route model.subModel
            else
                SubPage.init { turbulencePath = model.turbulenceImgSrc, csrfToken = model.csrfToken } route

        ( newTopModel, tCmd ) =
            if route == model.route then
                ( model.topModel, Cmd.none )
            else
                let
                    pipelineIdentifier =
                        pipelineIdentifierFromRouteOrModel route model
                in
                    ( { model
                        | route = route
                      }
                    , case pipelineIdentifier of
                        Nothing ->
                            TopBar.fetchUser

                        Just pid ->
                            Cmd.batch
                                [ Task.attempt PipelineFetched <|
                                    Concourse.Pipeline.fetchPipeline pid
                                , TopBar.fetchUser
                                ]
                    )
    in
        ( { model
            | navIndex = navIndex
            , subModel = newSubmodel
            , topModel = newTopModel
            , route = route
          }
        , Cmd.batch
            [ Cmd.map (SubMsg navIndex) cmd
            , Cmd.map (TopMsg navIndex) tCmd
            , resetFavicon
            ]
        )


resetFavicon : Cmd Msg
resetFavicon =
    Task.perform (always Noop) <|
        Favicon.set "/public/images/favicon.png"


view : Model -> Html Msg
view model =
    case model.subModel of
        SubPage.DashboardModel _ ->
            Html.map (SubMsg model.navIndex) (SubPage.view model.subModel)

        SubPage.DashboardHdModel _ ->
            Html.map (SubMsg model.navIndex) (SubPage.view model.subModel)

        _ ->
            Html.div [ class "content-frame" ]
                [ Html.div [ id "top-bar-app" ]
                    [ Html.nav
                        [ classList
                            [ ( "module-topbar", True )
                            , ( "top-bar", True )
                            , ( "test", True )
                            , ( "paused", Maybe.withDefault False <| Maybe.map .paused model.pipeline )
                            ]
                        ]
                        [ Html.div
                            [ classList [ ( "topbar-logo", True ) ] ]
                            [ Html.a [ class "logo-image-link", href "/" ] [] ]
                        , Html.ul [ class "groups" ] <| viewBreadcrumbs model.route.logical
                        , Html.div [ class "topbar-login" ]
                            [ Html.div [ class "topbar-user-info" ]
                                [ viewUserState model.topModel.userState model.topModel.userMenuVisible
                                ]
                            ]
                        ]
                    ]
                , Html.div [ class "bottom" ]
                    [ Html.div [ id "content" ]
                        [ Html.div [ id "subpage" ]
                            [ Html.map (SubMsg model.navIndex) (SubPage.view model.subModel) ]
                        ]
                    ]
                ]


viewUserState : TopBar.UserState -> Bool -> Html Msg
viewUserState userState userMenuVisible =
    case userState of
        TopBar.UserStateUnknown ->
            Html.text ""

        TopBar.UserStateLoggedOut ->
            Html.div [ class "user-id", onClick LogIn ]
                [ Html.a
                    [ href "/sky/login"
                    , Attributes.attribute "aria-label" "Log In"
                    , class "login-button"
                    ]
                    [ Html.text "login"
                    ]
                ]

        TopBar.UserStateLoggedIn user ->
            Html.div [ class "user-info" ]
                [ Html.div [ class "user-id", onClick TopMsg TopBar.ToggleUserMenu ]
                    [ Html.text <|
                        TopBar.userDisplayName user
                    ]
                , Html.div [ classList [ ( "user-menu", True ), ( "hidden", not userMenuVisible ) ], onClick LogOut ]
                    [ Html.a
                        [ Attributes.attribute "aria-label" "Log Out"
                        ]
                        [ Html.text "logout"
                        ]
                    ]
                ]


viewBreadcrumbs : Routes.Route -> List (Html Msg)
viewBreadcrumbs route =
    List.intersperse (Html.li [ class "nav-item" ] [ Html.text "/" ]) <|
        case route of
            Routes.Pipeline teamName pipelineName ->
                [ viewBreadcrumbPipeline pipelineName route ]

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


viewBreadcrumbPipeline : String -> Routes.Route -> Html Msg
viewBreadcrumbPipeline pipelineName route =
    let
        url =
            Routes.toString route
    in
        Html.li [ class "nav-item" ]
            [ Html.a
                [ StrictEvents.onLeftClick <| TopMsg TopBar.ResetToPipeline url
                , href url
                ]
                [ Html.div [ class "breadcrumb-icon breadcrumb-pipeline-icon" ] []
                , Html.text <| () decodeName pipelineName
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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.topBarType of
        Dashboard ->
            Sub.batch
                [ newUrl NewUrl
                , tokenReceived TokenReceived
                , Sub.map (SubMsg model.navIndex) <| SubPage.subscriptions model.subModel
                ]

        Normal ->
            Sub.batch
                [ newUrl NewUrl
                , tokenReceived TokenReceived
                , Sub.map (SubMsg model.navIndex) <| SubPage.subscriptions model.subModel
                , case pipelineIdentifierFromRouteOrModel model.route model of
                    Nothing ->
                        Sub.none

                    Just pid ->
                        Time.every (5 * Time.second) (always (FetchPipeline pid))
                , Time.every (5 * Time.second) (TopMsg TopBar.FetchUser)
                ]


pipelineIdentifierFromRouteOrModel : Routes.ConcourseRoute -> Model -> Maybe Concourse.PipelineIdentifier
pipelineIdentifierFromRouteOrModel route model =
    case extractPidFromRoute route.logical of
        Nothing ->
            case model.pipeline of
                Nothing ->
                    Nothing

                Just pipeline ->
                    Just { teamName = pipeline.teamName, pipelineName = pipeline.name }

        Just pidFromRoute ->
            Just pidFromRoute


routeMatchesModel : Routes.ConcourseRoute -> Model -> Bool
routeMatchesModel route model =
    case ( route.logical, model.subModel ) of
        ( Routes.Pipeline _ _, SubPage.PipelineModel _ ) ->
            True

        ( Routes.Resource _ _ _, SubPage.ResourceModel _ ) ->
            True

        ( Routes.Build _ _ _ _, SubPage.BuildModel _ ) ->
            True

        ( Routes.Job _ _ _, SubPage.JobModel _ ) ->
            True

        ( Routes.Dashboard, SubPage.DashboardModel _ ) ->
            True

        _ ->
            False
