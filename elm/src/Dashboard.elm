port module Dashboard exposing (Model, Msg, init, subscriptions, update, view)

import Char
import Concourse
import Concourse.Cli
import Concourse.Info
import Concourse.Job
import Concourse.Pipeline
import Concourse.PipelineStatus
import Concourse.Resource
import Concourse.User
import DashboardHelpers exposing (..)
import Dashboard.Group as Group
import Dashboard.Pipeline as Pipeline
import Dict exposing (Dict)
import Dom
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, classList, draggable, href, id, src)
import Html.Attributes.Aria exposing (ariaLabel)
import Http
import Keyboard
import List.Extra
import Mouse
import NewTopBar
import NoPipeline exposing (Msg, view)
import Regex exposing (HowMany(All), regex, replace)
import RemoteData
import Routes
import Simple.Fuzzy exposing (filter, match, root)
import Task exposing (Task)
import Time exposing (Time)
import UserState


type alias Ports =
    { title : String -> Cmd Msg
    }


port pinTeamNames : () -> Cmd msg


port tooltip : ( String, String ) -> Cmd msg


type alias Flags =
    { csrfToken : String
    , turbulencePath : String
    , search : String
    }


type alias Model =
    { topBar : NewTopBar.Model
    , userState : UserState.UserState
    , mPipelines : RemoteData.WebData (List Concourse.Pipeline)
    , pipelines : List Concourse.Pipeline
    , filteredPipelines : List Concourse.Pipeline
    , mJobs : RemoteData.WebData (List Concourse.Job)
    , pipelineJobs : Dict Int (List Concourse.Job)
    , pipelineResourceErrors : Dict ( String, String ) Bool
    , concourseVersion : String
    , csrfToken : String
    , turbulenceImgSrc : String
    , now : Maybe Time
    , showHelp : Bool
    , hideFooter : Bool
    , hideFooterCounter : Time
    , dragState : Pipeline.DragState
    , dropState : Pipeline.DropState
    }

type alias Data =
    { pipelines : List Concourse.Pipeline
    , jobs : List Concourse.Job
    , resources : List Concourse.Resource
    , version : String
    }


type Msg
    = Noop
    | UserFetched (RemoteData.WebData Concourse.User)
    | PipelinesResponse (RemoteData.WebData (List Concourse.Pipeline))
    | JobsResponse (RemoteData.WebData (List Concourse.Job))
    | ResourcesResponse (RemoteData.WebData (List Concourse.Resource))
    | VersionFetched (Result Http.Error String)
    | ClockTick Time.Time
    | AutoRefresh Time
    | ShowFooter
    | KeyPressed Keyboard.KeyCode
    | KeyDowns Keyboard.KeyCode
    | TopBarMsg NewTopBar.Msg
    | PipelinePauseToggled Concourse.Pipeline (Result Http.Error ())
    | PipelineMsg Pipeline.Msg


init : Ports -> Flags -> ( Model, Cmd Msg )
init ports flags =
    let
        ( topBar, topBarMsg ) =
            NewTopBar.init True flags.search
    in
        ( { topBar = topBar
          , userState = UserState.UserStateUnknown
          , mPipelines = RemoteData.NotAsked
          , pipelines = []
          , filteredPipelines = []
          , mJobs = RemoteData.NotAsked
          , pipelineJobs = Dict.empty
          , pipelineResourceErrors = Dict.empty
          , now = Nothing
          , csrfToken = flags.csrfToken
          , turbulenceImgSrc = flags.turbulencePath
          , concourseVersion = ""
          , showHelp = False
          , hideFooter = False
          , hideFooterCounter = 0
          , dragState = Pipeline.NotDragging
          , dropState = Pipeline.NotDropping
          }
        , Cmd.batch
            [ fetchPipelines
            , fetchUser
            , fetchVersion
            , getCurrentTime
            , Cmd.map TopBarMsg topBarMsg
            , pinTeamNames ()
            , ports.title <| "Dashboard" ++ " - "
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        reload =
            Cmd.batch <|
                (if model.mPipelines == RemoteData.Loading then
                    []
                 else
                    [ fetchPipelines ]
                )
                    ++ [ fetchUser ]
                    ++ [ fetchVersion, Cmd.map TopBarMsg NewTopBar.fetchUser ]
    in
        case msg of
            Noop ->
                ( model, Cmd.none )

            UserFetched user ->
                case user of
                    RemoteData.Success user ->
                        ( { model | userState = UserState.UserStateLoggedIn user }
                        , Cmd.none
                        )

                    _ ->
                        ( { model | userState = UserState.UserStateLoggedOut }
                        , Cmd.none
                        )

            PipelinesResponse response ->
                case response of
                    RemoteData.Success pipelines ->
                        ( { model | mPipelines = response, pipelines = pipelines }, Cmd.batch [ fetchAllJobs, fetchAllResources ] )

                    _ ->
                        ( model, Cmd.none )

            JobsResponse response ->
                case ( response, model.mPipelines ) of
                    ( RemoteData.Success jobs, RemoteData.Success pipelines ) ->
                        let
                            pipelineJobs =
                                jobsByPipelineId pipelines jobs
                        in
                            ( { model
                                | mJobs = response
                                , pipelineJobs = pipelineJobs
                                , filteredPipelines = filter model.topBar.query pipelines pipelineJobs
                              }
                            , Cmd.none
                            )

                    _ ->
                        ( model, Cmd.none )

            ResourcesResponse response ->
                case ( response, model.mPipelines ) of
                    ( RemoteData.Success resources, RemoteData.Success pipelines ) ->
                        ( { model | pipelineResourceErrors = resourceErrorsByPipelineIdentifier resources }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            VersionFetched (Ok version) ->
                ( { model | concourseVersion = version }, Cmd.none )

            VersionFetched (Err err) ->
                ( { model | concourseVersion = "" }, Cmd.none )

            ClockTick now ->
                if model.hideFooterCounter + Time.second > 5 * Time.second then
                    ( { model | now = Just now, hideFooter = True }, Cmd.none )
                else
                    ( { model | now = Just now, hideFooterCounter = model.hideFooterCounter + Time.second }, Cmd.none )

            AutoRefresh _ ->
                ( model
                , reload
                )

            KeyPressed keycode ->
                handleKeyPressed (Char.fromCode keycode) model

            KeyDowns keycode ->
                update (TopBarMsg (NewTopBar.KeyDown keycode)) model

            ShowFooter ->
                ( { model | hideFooter = False, hideFooterCounter = 0 }, Cmd.none )

            TopBarMsg msg ->
                let
                    ( newTopBar, newTopBarMsg ) =
                        NewTopBar.update msg model.topBar

                    newModel =
                        case msg of
                            NewTopBar.FilterMsg query ->
                                { model
                                    | topBar = newTopBar
                                    , filteredPipelines = filter query model.pipelines model.pipelineJobs
                                }

                            NewTopBar.KeyDown keycode ->
                                if keycode == 13 then
                                    { model
                                        | topBar = newTopBar
                                        , filteredPipelines = filter newTopBar.query model.pipelines model.pipelineJobs
                                    }
                                else
                                    { model | topBar = newTopBar }

                            _ ->
                                { model | topBar = newTopBar }

                    newMsg =
                        case msg of
                            NewTopBar.LoggedOut (Ok _) ->
                                reload

                            _ ->
                                Cmd.map TopBarMsg newTopBarMsg
                in
                    ( newModel, newMsg )

            PipelineMsg (Pipeline.TogglePipelinePaused pipeline) ->
                ( model, togglePipelinePaused pipeline model.csrfToken )

            PipelinePauseToggled pipeline (Ok ()) ->
                let
                    togglePipelinePause : List Concourse.Pipeline -> List Concourse.Pipeline
                    togglePipelinePause pipelines =
                        List.Extra.updateIf
                            ((==) pipeline)
                            (\pipeline -> { pipeline | paused = not pipeline.paused })
                            pipelines
                in
                    ( { model
                        | filteredPipelines = togglePipelinePause model.filteredPipelines
                      }
                    , Cmd.none
                    )

            PipelinePauseToggled _ (Err _) ->
                ( model, Cmd.none )

            PipelineMsg (Pipeline.DragStart teamName index) ->
                ( { model | dragState = Pipeline.Dragging teamName index }, Cmd.none )

            PipelineMsg (Pipeline.DragOver teamName index) ->
                ( { model | dropState = Pipeline.Dropping index }, Cmd.none )

            PipelineMsg (Pipeline.Tooltip pipelineName teamName) ->
                ( model, tooltip ( pipelineName, teamName ) )

            PipelineMsg Pipeline.DragEnd ->
                case ( model.dragState, model.dropState ) of
                    ( Pipeline.Dragging teamName dragIndex, Pipeline.Dropping dropIndex ) ->
                        let
                            shiftPipelines : List Concourse.Pipeline -> List Concourse.Pipeline
                            shiftPipelines pipelines =
                                if dragIndex == dropIndex then
                                    pipelines
                                else
                                    case
                                        List.head <|
                                            List.drop dragIndex <|
                                                List.filter ((==) teamName << .teamName) pipelines
                                    of
                                        Nothing ->
                                            pipelines

                                        Just pipeline ->
                                            shiftPipelineTo pipeline dropIndex pipelines

                            filteredPipelines =
                                shiftPipelines model.filteredPipelines
                        in
                            ( { model
                                | filteredPipelines = filteredPipelines
                                , dragState = Pipeline.NotDragging
                                , dropState = Pipeline.NotDropping
                              }
                            , orderPipelines teamName filteredPipelines model.csrfToken
                            )

                    _ ->
                        ( { model | dragState = Pipeline.NotDragging, dropState = Pipeline.NotDropping }, Cmd.none )


shiftPipelineTo : Concourse.Pipeline -> Int -> List Concourse.Pipeline -> List Concourse.Pipeline
shiftPipelineTo pipeline position pipelines =
    case pipelines of
        [] ->
            if position < 0 then
                []
            else
                [ pipeline ]

        p :: ps ->
            if p.teamName /= pipeline.teamName then
                p :: shiftPipelineTo pipeline position ps
            else if p == pipeline then
                shiftPipelineTo pipeline (position - 1) ps
            else if position == 0 then
                pipeline :: p :: shiftPipelineTo pipeline (position - 1) ps
            else
                p :: shiftPipelineTo pipeline (position - 1) ps


orderPipelines : String -> List Concourse.Pipeline -> Concourse.CSRFToken -> Cmd Msg
orderPipelines teamName pipelines csrfToken =
    Task.attempt (always Noop) <|
        Concourse.Pipeline.order
            teamName
            (List.map .name <| List.filter ((==) teamName << .teamName) pipelines)
            csrfToken


togglePipelinePaused : Concourse.Pipeline -> Concourse.CSRFToken -> Cmd Msg
togglePipelinePaused pipeline csrfToken =
    Task.attempt (PipelinePauseToggled pipeline) <|
        if pipeline.paused then
            Concourse.Pipeline.unpause pipeline.teamName pipeline.name csrfToken
        else
            Concourse.Pipeline.pause pipeline.teamName pipeline.name csrfToken


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second ClockTick
        , Time.every (5 * Time.second) AutoRefresh
        , Mouse.moves (\_ -> ShowFooter)
        , Mouse.clicks (\_ -> ShowFooter)
        , Keyboard.presses KeyPressed
        , Keyboard.downs KeyDowns
        ]


view : Model -> Html Msg
view model =
    Html.div [ class "page" ]
        [ Html.map TopBarMsg (NewTopBar.view model.topBar)
        , dashboardView model
        ]


dashboardView : Model -> Html Msg
dashboardView model =
    case ( model.mPipelines, model.mJobs ) of
        ( RemoteData.Success [], _ ) ->
            Html.map (\_ -> Noop) NoPipeline.view

        ( RemoteData.Success _, RemoteData.Success _ ) ->
            pipelinesView model

        ( RemoteData.Failure _, _ ) ->
            turbulenceView model

        ( _, RemoteData.Failure _ ) ->
            turbulenceView model

        _ ->
            Html.text ""


noResultsView : String -> Html Msg
noResultsView query =
    let
        boldedQuery =
            Html.span [ class "monospace-bold" ] [ Html.text query ]
    in
        Html.div
            [ class "dashboard" ]
            [ Html.div [ class "dashboard-content " ]
                [ Html.div
                    [ class "dashboard-team-group" ]
                    [ Html.div [ class "pin-wrapper" ]
                        [ Html.div [ class "dashboard-team-name no-results" ]
                            [ Html.text "No results for "
                            , boldedQuery
                            , Html.text " matched your search."
                            ]
                        ]
                    ]
                ]
            ]


helpView : Model -> Html Msg
helpView model =
    Html.div
        [ classList
            [ ( "keyboard-help", True )
            , ( "hidden", not model.showHelp )
            ]
        ]
        [ Html.div [ class "help-title" ] [ Html.text "keyboard shortcuts" ]
        , Html.div [ class "help-line" ] [ Html.div [ class "keys" ] [ Html.span [ class "key" ] [ Html.text "/" ] ], Html.text "search" ]
        , Html.div [ class "help-line" ] [ Html.div [ class "keys" ] [ Html.span [ class "key" ] [ Html.text "?" ] ], Html.text "hide/show help" ]
        ]


footerView : Model -> Html Msg
footerView model =
    Html.div
        [ if model.hideFooter || model.showHelp then
            class "dashboard-footer hidden"
          else
            class "dashboard-footer"
        ]
        [ Html.div [ class "dashboard-legend" ]
            [ Html.div [ class "dashboard-status-pending" ]
                [ Html.div [ class "dashboard-pipeline-icon" ] [], Html.text "pending" ]
            , Html.div [ class "dashboard-paused" ]
                [ Html.div [ class "dashboard-pipeline-icon" ] [], Html.text "paused" ]
            , Html.div [ class "dashboard-running" ]
                [ Html.div [ class "dashboard-pipeline-icon" ] [], Html.text "running" ]
            , Html.div [ class "dashboard-status-failed" ]
                [ Html.div [ class "dashboard-pipeline-icon" ] [], Html.text "failing" ]
            , Html.div [ class "dashboard-status-errored" ]
                [ Html.div [ class "dashboard-pipeline-icon" ] [], Html.text "errored" ]
            , Html.div [ class "dashboard-status-aborted" ]
                [ Html.div [ class "dashboard-pipeline-icon" ] [], Html.text "aborted" ]
            , Html.div [ class "dashboard-status-succeeded" ]
                [ Html.div [ class "dashboard-pipeline-icon" ] [], Html.text "succeeded" ]
            , Html.div [ class "dashboard-status-separator" ] [ Html.text "|" ]
            , Html.div [ class "dashboard-high-density" ]
                [ Html.a [ class "toggle-high-density", href Routes.dashboardHdRoute, ariaLabel "Toggle high-density view" ]
                    [ Html.div [ class "dashboard-pipeline-icon hd-off" ] [], Html.text "high-density" ]
                ]
            ]
        , Html.div [ class "concourse-info" ]
            [ Html.div [ class "concourse-version" ]
                [ Html.text "version: v", Html.text model.concourseVersion ]
            , Html.div [ class "concourse-cli" ]
                [ Html.text "cli: "
                , Html.a [ href (Concourse.Cli.downloadUrl "amd64" "darwin"), ariaLabel "Download OS X CLI" ]
                    [ Html.i [ class "fa fa-apple" ] [] ]
                , Html.a [ href (Concourse.Cli.downloadUrl "amd64" "windows"), ariaLabel "Download Windows CLI" ]
                    [ Html.i [ class "fa fa-windows" ] [] ]
                , Html.a [ href (Concourse.Cli.downloadUrl "amd64" "linux"), ariaLabel "Download Linux CLI" ]
                    [ Html.i [ class "fa fa-linux" ] [] ]
                ]
            ]
        ]


turbulenceView : Model -> Html Msg
turbulenceView model =
    Html.div
        [ class "error-message" ]
        [ Html.div [ class "message" ]
            [ Html.img [ src model.turbulenceImgSrc, class "seatbelt" ] []
            , Html.p [] [ Html.text "experiencing turbulence" ]
            , Html.p [ class "explanation" ] []
            ]
        ]



-- sorting:
-- 1. If logged in:
--  1.1. fistly, teams you're a member of; then
--    1.1.1 alphabetically sort pipelines
--  1.2. teams you're not a member of.
--    1.2.1 alphabetically sort pipelines
--
-- In the end, it's all about
--
--
--
--
--
--


filterOnFuzzyMatchingTerm : String -> List Group.Group -> List Group.Group
filterOnFuzzyMatchingTerm term groups =
    fuzzySearch (.team >> .name) term groups


pipelinesView : Model -> Html Msg
pipelinesView model =
    let
        pipelines =
            pipelinesWithJobs model.pipelineJobs model.pipelineResourceErrors model.filteredPipelines

        teams =
            RemoteData.withDefault [] model.topBar.teams

        allGroups =
            Group.groups { allPipelines = pipelines, teams = teams }

        teamFilters =
            filterTerms model.topBar.query |> List.filter (String.startsWith "team:") |> List.map (String.dropLeft 5)

        filteredGroups =
            List.foldl filterOnFuzzyMatchingTerm allGroups teamFilters

        groupViews =
            if List.isEmpty teamFilters || not (List.all (String.startsWith "team:") (filterTerms model.topBar.query)) then
                List.map (Group.view model.dragState model.dropState model.now) <| List.filter (not << List.isEmpty << .pipelines) <| filteredGroups
            else
                List.map (Group.view model.dragState model.dropState model.now) filteredGroups
    in
        if List.isEmpty groupViews then
            noResultsView (toString model.topBar.query)
        else
            Html.div
                [ class "dashboard" ]
            <|
                [ Html.div [ class "dashboard-content" ] <| (List.map (Html.map PipelineMsg) groupViews)
                , footerView model
                , helpView model
                ]


handleKeyPressed : Char -> Model -> ( Model, Cmd Msg )
handleKeyPressed key model =
    case key of
        '/' ->
            ( model, Task.attempt (always Noop) (Dom.focus "search-input-field") )

        '?' ->
            ( { model | showHelp = not model.showHelp }, Cmd.none )

        _ ->
            update ShowFooter model

fetchDataCmd : RemoteData.WebData Data
    RemoteData.map Data (RemoteData.fromTask Concourse.User.fetchUser)
        |> RemoteData.andMap (RemoteData.fromTask Concourse.Pipeline.fetchPipelines)
        |> RemoteData.andMap (RemoteData.fromTask Concourse.Job.fetchAllJobs)
        |> RemoteData.andMap (RemoteData.fromTask Concourse.Info.fetch)

fetchUser : Cmd Msg
fetchUser =
    Cmd.map UserFetched <|
        RemoteData.asCmd Concourse.User.fetchUser


fetchPipelines : Cmd Msg
fetchPipelines =
    Cmd.map PipelinesResponse <|
        RemoteData.asCmd Concourse.Pipeline.fetchPipelines


fetchAllJobs : Cmd Msg
fetchAllJobs =
    Cmd.map JobsResponse <|
        RemoteData.asCmd Concourse.Job.fetchAllJobs


fetchAllResources : Cmd Msg
fetchAllResources =
    Cmd.map ResourcesResponse <|
        RemoteData.asCmd Concourse.Resource.fetchAllResources


fetchVersion : Cmd Msg
fetchVersion =
    Concourse.Info.fetch
        |> Task.map .version
        |> Task.attempt VersionFetched


getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform ClockTick Time.now


filter : String -> List Concourse.Pipeline -> Dict PipelineId (List Concourse.Job) -> List Concourse.Pipeline
filter query pipelines pipelineJobs =
    filterByTerms (filterTerms query) pipelines pipelineJobs


filterTerms : String -> List String
filterTerms query =
    query
        |> replace All (regex "team:\\s*") (\_ -> "team:")
        |> replace All (regex "status:\\s*") (\_ -> "status:")
        |> String.words


filterByTerms : List String -> List Concourse.Pipeline -> Dict PipelineId (List Concourse.Job) -> List Concourse.Pipeline
filterByTerms terms pipelines pipelineJobs =
    case terms of
        [] ->
            pipelines

        x :: xs ->
            filterByTerms xs (filterByTerm x (pipelinesWithJobs pipelineJobs Dict.empty pipelines)) pipelineJobs


filterByTerm : String -> List PipelineWithJobs -> List Concourse.Pipeline
filterByTerm term pipelines =
    let
        searchTeams =
            String.startsWith "team:" term

        searchStatus =
            String.startsWith "status:" term

        teamSearchTerm =
            if searchTeams then
                String.dropLeft 5 term
            else
                term

        statusSearchTerm =
            if searchStatus then
                String.dropLeft 7 term
            else
                term

        plist =
            List.map (\p -> p.pipeline) pipelines

        filterByStatus =
            fuzzySearch (\p -> Pipeline.pipelineStatus p |> Concourse.PipelineStatus.show) statusSearchTerm pipelines
    in
        if searchTeams then
            fuzzySearch .teamName teamSearchTerm plist
        else if searchStatus then
            List.map (\p -> p.pipeline) filterByStatus
        else
            fuzzySearch .name term plist


fuzzySearch : (a -> String) -> String -> List a -> List a
fuzzySearch map needle records =
    let
        negateSearch =
            String.startsWith "-" needle
    in
        if negateSearch then
            List.filter (not << Simple.Fuzzy.match needle << map) records
        else
            List.filter (Simple.Fuzzy.match needle << map) records
