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
import Concourse.Team
import DashboardHelpers exposing (..)
import Dashboard.Group as Group
import Dashboard.GroupWithTag as GroupWithTag
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
import Set
import Simple.Fuzzy exposing (filter, match, root)
import Task
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
    , data : RemoteData.WebData DashboardData
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


type DashboardData
    = Authenticated (List GroupWithTag.GroupWithTag) Concourse.User
    | Unauthenticated (List Group.Group)


type alias Data =
    { teams : List Concourse.Team
    , pipelines : List Concourse.Pipeline
    , jobs : List Concourse.Job
    , resources : List Concourse.Resource
    , version : String
    }


type Msg
    = Noop
    | DataFetched (RemoteData.WebData DashboardData)
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
          , data = RemoteData.NotAsked
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
            [ fetchData
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
                (case model.data of
                    RemoteData.Success _ ->
                        [ fetchData ]

                    _ ->
                        []
                )
                    ++ [ Cmd.map TopBarMsg NewTopBar.fetchUser ]
    in
        case msg of
            Noop ->
                ( model, Cmd.none )

            DataFetched remoteData ->
                ( { model | data = remoteData }
                , Cmd.none
                )

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

                                    -- , filteredPipelines = filter query model.pipelines model.pipelineJobs
                                }

                            NewTopBar.KeyDown keycode ->
                                if keycode == 13 then
                                    { model
                                        | topBar = newTopBar

                                        -- , filteredPipelines = filter newTopBar.query model.pipelines model.pipelineJobs
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
                            shiftPipelines : List PipelineWithJobs -> List PipelineWithJobs
                            shiftPipelines pipelines =
                                if dragIndex == dropIndex then
                                    pipelines
                                else
                                    case
                                        List.head <|
                                            List.drop dragIndex <|
                                                pipelines
                                    of
                                        Nothing ->
                                            pipelines

                                        Just pipeline ->
                                            shiftPipelineTo pipeline dropIndex pipelines

                            filteredPipelines =
                                groups model
                                    |> List.Extra.find (.teamName >> (==) teamName)
                                    |> Maybe.map (.pipelines >> shiftPipelines)

                            newGroups =
                                filteredPipelines
                                    |> Maybe.map (\fps -> groups model |> List.Extra.updateIf (.teamName >> (==) teamName) (\g -> { g | pipelines = fps }))
                                    |> Maybe.withDefault (groups model)

                            newModel =
                                setGroups model newGroups
                        in
                            ( { newModel
                                | dragState = Pipeline.NotDragging
                                , dropState = Pipeline.NotDropping
                              }
                            , filteredPipelines
                                |> Maybe.map (\ps -> orderPipelines teamName ps model.csrfToken)
                                |> Maybe.withDefault reload
                            )

                    _ ->
                        ( { model | dragState = Pipeline.NotDragging, dropState = Pipeline.NotDropping }, Cmd.none )


setGroups : Model -> List Group.Group -> Model
setGroups model groups =
    case model.data of
        RemoteData.Success dd ->
            case dd of
                Authenticated _ u ->
                    { model | data = RemoteData.succeed (Authenticated (GroupWithTag.addTags u groups) u) }

                Unauthenticated _ ->
                    { model | data = RemoteData.succeed (Unauthenticated groups) }

        _ ->
            model


groupsFromDD : DashboardData -> List Group.Group
groupsFromDD dd =
    case dd of
        Authenticated gwts _ ->
            gwts |> List.map .group

        Unauthenticated gs ->
            gs


groups : Model -> List Group.Group
groups model =
    model.data |> RemoteData.map groupsFromDD |> RemoteData.withDefault []


shiftPipelineTo : PipelineWithJobs -> Int -> List PipelineWithJobs -> List PipelineWithJobs
shiftPipelineTo ({ pipeline } as pipelineWithJobs) position pipelines =
    case pipelines of
        [] ->
            if position < 0 then
                []
            else
                [ pipelineWithJobs ]

        p :: ps ->
            if p.pipeline.teamName /= pipeline.teamName then
                p :: shiftPipelineTo pipelineWithJobs position ps
            else if p.pipeline == pipeline then
                shiftPipelineTo pipelineWithJobs (position - 1) ps
            else if position == 0 then
                pipelineWithJobs :: p :: shiftPipelineTo pipelineWithJobs (position - 1) ps
            else
                p :: shiftPipelineTo pipelineWithJobs (position - 1) ps


orderPipelines : String -> List PipelineWithJobs -> Concourse.CSRFToken -> Cmd Msg
orderPipelines teamName pipelines csrfToken =
    Task.attempt (always Noop) <|
        Concourse.Pipeline.order
            teamName
            (List.map (.name << .pipeline) <| pipelines)
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
    case model.data of
        RemoteData.Success d ->
            let
                groups =
                    case d of
                        Authenticated gwts u ->
                            gwts |> List.map .group

                        Unauthenticated gs ->
                            gs

                pipelines =
                    groups |> List.concatMap .pipelines
            in
                case pipelines of
                    [] ->
                        Html.map (always Noop) NoPipeline.view

                    _ ->
                        pipelinesView model

        RemoteData.Failure _ ->
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


pipelinesView : Model -> Html Msg
pipelinesView model =
    let
        allGroups =
            model.data
                |> RemoteData.map
                    (\d ->
                        case d of
                            Unauthenticated gs ->
                                gs

                            Authenticated gwts _ ->
                                gwts |> List.map .group
                    )
                |> RemoteData.withDefault []

        teamFilters =
            filterTerms model.topBar.query |> List.filter (String.startsWith "team:") |> List.map (String.dropLeft 5)

        filteredGroups =
            filter model.topBar.query allGroups

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


fetchData : Cmd Msg
fetchData =
    remoteData |> Task.mapError (\e -> flip always (Debug.log "e" e) e) |> Task.andThen remoteUser |> RemoteData.asCmd |> Cmd.map DataFetched


allPipelines : Data -> List PipelineWithJobs
allPipelines data =
    flip always (Debug.log "data" data) <|
        (data.pipelines
            |> List.map
                (\p ->
                    { pipeline = p
                    , jobs =
                        data.jobs
                            |> List.filter
                                (\j ->
                                    (j.teamName == p.teamName)
                                        && (j.pipelineName == p.name)
                                )
                    , resourceError =
                        data.resources
                            |> List.any
                                (\r ->
                                    (r.teamName == p.teamName)
                                        && (r.pipelineName == p.name)
                                        && r.failingToCheck
                                )
                    }
                )
        )


remoteUser : Data -> Task.Task Http.Error DashboardData
remoteUser d =
    let
        data =
            { allPipelines = allPipelines d
            , teamNames =
                Set.union
                    (Set.fromList (List.map .teamName d.pipelines))
                    (Set.fromList (List.map .name d.teams))
                    |> Set.toList
            }
    in
        Concourse.User.fetchUser
            |> Task.map
                (\u ->
                    Authenticated
                        (GroupWithTag.groupsWithTags
                            { user = u
                            , data = data
                            }
                        )
                        u
                )
            |> Task.onError
                (\e ->
                    Task.succeed <|
                        Unauthenticated <|
                            Group.groups data
                )


remoteData : Task.Task Http.Error Data
remoteData =
    Task.map5 Data
        Concourse.Team.fetchTeams
        Concourse.Pipeline.fetchPipelines
        (Concourse.Job.fetchAllJobs |> Task.map (Maybe.withDefault []))
        (Concourse.Resource.fetchAllResources |> Task.map (Maybe.withDefault []))
        (Concourse.Info.fetch |> Task.map .version)


getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform ClockTick Time.now


filterTerms : String -> List String
filterTerms =
    replace All (regex "team:\\s*") (\_ -> "team:")
        >> replace All (regex "status:\\s*") (\_ -> "status:")
        >> String.words


filter : String -> List Group.Group -> List Group.Group
filter =
    filterTerms >> flip (List.foldl filterGroupsByTerm)


filterPipelinesByTerm : String -> Group.Group -> Group.Group
filterPipelinesByTerm term ({ pipelines } as group) =
    let
        searchStatus =
            String.startsWith "status:" term

        statusSearchTerm =
            if searchStatus then
                String.dropLeft 7 term
            else
                term

        filterByStatus =
            fuzzySearch (Pipeline.pipelineStatus >> Concourse.PipelineStatus.show) statusSearchTerm pipelines
    in
        { group
            | pipelines =
                if searchStatus then
                    filterByStatus
                else
                    fuzzySearch (.pipeline >> .name) term pipelines
        }


filterGroupsByTerm : String -> List Group.Group -> List Group.Group
filterGroupsByTerm term groups =
    let
        searchTeams =
            String.startsWith "team:" term

        teamSearchTerm =
            if searchTeams then
                String.dropLeft 5 term
            else
                term
    in
        if searchTeams then
            fuzzySearch .teamName teamSearchTerm groups
        else
            groups |> List.map (filterPipelinesByTerm term)


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
