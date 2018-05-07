port module SpaceLayout exposing (Flags, Model, Msg, init, locationMsg, subscriptions, update, view)

import SpaceRoutes
import SpaceSubPage
import SpaceTopBar
import Favicon
import Html exposing (Html)
import Html.Attributes as Attributes exposing (class, id)
import Navigation
import Task exposing (Task)


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
    , subModel : SpaceSubPage.Model
    , topModel : SpaceTopBar.Model
    , turbulenceImgSrc : String
    , notFoundImgSrc : String
    , csrfToken : String
    , route : SpaceRoutes.ConcourseRoute
    }


type Msg
    = Noop
    | RouteChanged SpaceRoutes.ConcourseRoute
    | SpaceSubMsg NavIndex SpaceSubPage.Msg
    | SpaceTopMsg NavIndex SpaceTopBar.Msg
    | NewUrl String
    | ModifyUrl String
    | SaveToken String
    | LoadToken
    | TokenReceived (Maybe String)


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        route =
            SpaceRoutes.parsePath location

        ( subModel, subCmd ) =
            SpaceSubPage.init flags.turbulenceImgSrc route

        ( topModel, topCmd ) =
            SpaceTopBar.init route

        navIndex =
            1

        model =
            { navIndex = navIndex
            , subModel = subModel
            , topModel = topModel
            , turbulenceImgSrc = flags.turbulenceImgSrc
            , notFoundImgSrc = flags.notFoundImgSrc
            , route = route
            , csrfToken = flags.csrfToken
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
                Navigation.modifyUrl (SpaceRoutes.customToString route)
    in
        ( model
        , Cmd.batch
            [ handleTokenCmd
            , stripCSRFTokenParamCmd
            , Cmd.map (SpaceSubMsg navIndex) subCmd
            , Cmd.map (SpaceTopMsg navIndex) topCmd
            ]
        )


locationMsg : Navigation.Location -> Msg
locationMsg =
    RouteChanged << SpaceRoutes.parsePath


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
                    SpaceSubPage.update model.turbulenceImgSrc model.notFoundImgSrc tokenValue (SpaceSubPage.NewCSRFToken tokenValue) model.subModel
            in
                ( { model
                    | csrfToken = tokenValue
                    , subModel = newSubModel
                  }
                , Cmd.map (SpaceSubMsg anyNavIndex) subCmd
                )

        SpaceSubMsg navIndex (SpaceSubPage.PipelinesFetched (Ok pipelines)) ->
            let
                pipeline =
                    List.head pipelines

                ( subModel, subCmd ) =
                    SpaceSubPage.update
                        model.turbulenceImgSrc
                        model.notFoundImgSrc
                        model.csrfToken
                        (SpaceSubPage.DefaultPipelineFetched pipeline)
                        model.subModel
            in
                case pipeline of
                    Nothing ->
                        ( { model
                            | subModel = subModel
                          }
                        , Cmd.map (SpaceSubMsg navIndex) subCmd
                        )

                    Just p ->
                        ( { model | subModel = subModel }, Cmd.map (SpaceSubMsg navIndex) subCmd )

        -- otherwise, pass down
        SpaceSubMsg navIndex m ->
            if validNavIndex model.navIndex navIndex then
                let
                    ( subModel, subCmd ) =
                        SpaceSubPage.update model.turbulenceImgSrc model.notFoundImgSrc model.csrfToken m model.subModel
                in
                    ( { model | subModel = subModel }, Cmd.map (SpaceSubMsg navIndex) subCmd )
            else
                ( model, Cmd.none )

        SpaceTopMsg navIndex m ->
            if validNavIndex model.navIndex navIndex then
                let
                    ( topModel, topCmd ) =
                        SpaceTopBar.update m model.topModel
                in
                    ( { model | topModel = topModel }, Cmd.map (SpaceTopMsg navIndex) topCmd )
            else
                ( model, Cmd.none )

        Noop ->
            ( model, Cmd.none )


validNavIndex : NavIndex -> NavIndex -> Bool
validNavIndex modelNavIndex navIndex =
    if navIndex == anyNavIndex then
        True
    else
        navIndex == modelNavIndex


urlUpdate : SpaceRoutes.ConcourseRoute -> Model -> ( Model, Cmd Msg )
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
                SpaceSubPage.urlUpdate route model.subModel
            else
                SpaceSubPage.init model.turbulenceImgSrc route

        ( newTopModel, tCmd ) =
            if route == model.route then
                ( model.topModel, Cmd.none )
            else
                SpaceTopBar.urlUpdate route model.topModel
    in
        ( { model
            | navIndex = navIndex
            , subModel = newSubmodel
            , topModel = newTopModel
            , route = route
          }
        , Cmd.batch
            [ Cmd.map (SpaceSubMsg navIndex) cmd
            , Cmd.map (SpaceTopMsg navIndex) tCmd
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
        _ ->
            Html.div [ class "content-frame" ]
                [ Html.div [ id "top-bar-app" ]
                    [ Html.map (SpaceTopMsg model.navIndex) (SpaceTopBar.view model.topModel) ]
                , Html.div [ class "bottom" ]
                    [ Html.div [ id "content" ]
                        [ Html.div [ id "SpaceSubPage" ]
                            [ Html.map (SpaceSubMsg model.navIndex) (SpaceSubPage.view model.subModel) ]
                        ]
                    ]
                ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ newUrl NewUrl
        , tokenReceived TokenReceived
        , Sub.map (SpaceSubMsg model.navIndex) <| SpaceSubPage.subscriptions model.subModel
        ]


routeMatchesModel : SpaceRoutes.ConcourseRoute -> Model -> Bool
routeMatchesModel route model =
    case ( route.logical, model.subModel ) of
        ( SpaceRoutes.SpacePipeline _ _, SpaceSubPage.SpacePipelineModel _ ) ->
            True

        _ ->
            False
