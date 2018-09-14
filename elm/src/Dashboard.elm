port module Dashboard exposing (Model, Msg, init, subscriptions, update, view)

import Char
import Concourse
import Concourse.Cli
import Concourse.Pipeline
import Concourse.PipelineStatus
import Concourse.User
import Dashboard.Group as Group
import Dashboard.GroupWithTag as GroupWithTag
import Dashboard.Pipeline as Pipeline
import Dom
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, classList, draggable, href, id, src)
import Html.Attributes.Aria exposing (ariaLabel)
import Http
import Keyboard
import List.Extra
import Maybe.Extra
import Mouse
import Monocle.Iso
import Monocle.Optional
import Monocle.Lens
import NewTopBar
import NoPipeline exposing (Msg, view)
import Regex exposing (HowMany(All), regex, replace)
import RemoteData
import Routes
import Simple.Fuzzy exposing (filter, match, root)
import Task
import Time exposing (Time)


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



-- TODO the word "State" is a smell. what is this thing really?
-- it's a glorified Result/Either type of functor, just with multiple
-- "error" cases. Maybe it makes more sense to use one of those types,
-- as they are pretty descriptive.


type DashboardState
    = NotAsked
    | Turbulence String
    | NoPipelines
    | HasData SubState


type alias SubState =
    { csrfToken : String -- static config, i don't even think this thing gets fetched. maybe it comes from a port? try the thunk trick
    , dragState : Group.DragState -- move to Group? in a sense there is a global dragstate, it's just that group is the only one really affected by it.
    , dropState : Group.DropState -- ditto, plus maybe the whole drag/drop types will be refactored
    , hideFooter : Bool
    , hideFooterCounter : Time
    , now : Time
    , teamData : TeamData
    }


type alias Model =
    { csrfToken : String
    , state : DashboardState
    , topBar : NewTopBar.Model
    , turbulencePath : String -- this doesn't vary, it's more a prop (in the sense of react) than state. should be a way to use a thunk for the Turbulence case of DashboardState
    , showHelp : Bool
    }


type alias Config =
    { csrfToken : String
    , turbulencePath : String
    }


type alias Modifier =
    { showHelp : Bool
    , dragState : Group.DragState
    , dropState : Group.DropState
    , hideFooter : Bool
    , hideFooterCounter : Time
    }


type TeamData
    = Unauthenticated
        { apiData : Group.APIData
        }
    | Authenticated
        { apiData : Group.APIData
        , user : Concourse.User
        }


teamApiData : TeamData -> Group.APIData
teamApiData teamData =
    case teamData of
        Unauthenticated { apiData } ->
            apiData

        Authenticated { apiData } ->
            apiData


type Msg
    = Noop
    | APIDataFetched (RemoteData.WebData ( Time.Time, ( Group.APIData, Maybe Concourse.User ) ))
    | ClockTick Time.Time
    | AutoRefresh Time
    | ShowFooter
    | KeyPressed Keyboard.KeyCode
    | KeyDowns Keyboard.KeyCode
    | TopBarMsg NewTopBar.Msg
    | PipelinePauseToggled Concourse.Pipeline (Result Http.Error ())
    | PipelineMsg Pipeline.Msg
    | GroupMsg Group.Msg


init : Ports -> Flags -> ( Model, Cmd Msg )
init ports flags =
    let
        ( topBar, topBarMsg ) =
            NewTopBar.init True flags.search
    in
        ( { state = NotAsked
          , topBar = topBar
          , csrfToken = flags.csrfToken
          , turbulencePath = flags.turbulencePath
          , showHelp = False
          }
        , Cmd.batch
            [ fetchData
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
                (case model.state of
                    HasData _ ->
                        [ fetchData ]

                    _ ->
                        []
                )
                    ++ [ Cmd.map TopBarMsg NewTopBar.fetchUser ]
    in
        case msg of
            Noop ->
                ( model, Cmd.none )

            -- TODO this case is a bit long, with a lot of non-value-added whitespace.
            -- most likely it will be better to use a higher-order function a la Result.map
            APIDataFetched remoteData ->
                let
                    state =
                        case remoteData of
                            RemoteData.NotAsked ->
                                NotAsked

                            RemoteData.Loading ->
                                NotAsked

                            RemoteData.Failure _ ->
                                Turbulence ""

                            RemoteData.Success ( now, ( apiData, user ) ) ->
                                case apiData.pipelines of
                                    [] ->
                                        NoPipelines

                                    _ ->
                                        let
                                            teamData =
                                                user
                                                    |> Maybe.map (\u -> Authenticated { apiData = apiData, user = u })
                                                    |> Maybe.withDefault (Unauthenticated { apiData = apiData })
                                        in
                                            HasData
                                                { teamData = teamData
                                                , now = now
                                                , hideFooter = False
                                                , hideFooterCounter = 0
                                                , dragState = Group.NotDragging
                                                , dropState = Group.NotDropping
                                                , csrfToken = model.csrfToken
                                                }
                in
                    ( { model | state = state }
                    , Cmd.none
                    )

            ClockTick now ->
                case model.state of
                    HasData substate ->
                        if substate.hideFooterCounter + Time.second > 5 * Time.second then
                            ( { model | state = HasData { substate | now = now, hideFooter = True } }, Cmd.none )
                        else
                            ( { model | state = HasData { substate | now = now, hideFooterCounter = substate.hideFooterCounter + Time.second } }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            AutoRefresh _ ->
                ( model
                , reload
                )

            KeyPressed keycode ->
                handleKeyPressed (Char.fromCode keycode) model

            KeyDowns keycode ->
                update (TopBarMsg (NewTopBar.KeyDown keycode)) model

            ShowFooter ->
                case model.state of
                    HasData substate ->
                        ( { model | state = HasData { substate | hideFooter = False, hideFooterCounter = 0 } }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            -- TODO pull the topbar logic right in here. right now there are wasted API calls and this crufty
            -- nonsense going on. however, this feels like a big change and not a big burning fire
            TopBarMsg msg ->
                let
                    ( newTopBar, newTopBarMsg ) =
                        NewTopBar.update msg model.topBar

                    newMsg =
                        case msg of
                            NewTopBar.LoggedOut (Ok _) ->
                                reload

                            _ ->
                                Cmd.map TopBarMsg newTopBarMsg
                in
                    ( { model | topBar = newTopBar }, newMsg )

            PipelineMsg (Pipeline.TogglePipelinePaused pipeline) ->
                ( model, togglePipelinePaused pipeline model.csrfToken )

            PipelinePauseToggled pipeline (Ok ()) ->
                let
                    togglePipelinePause : List Concourse.Pipeline -> List Concourse.Pipeline
                    togglePipelinePause pipelines =
                        List.Extra.updateIf
                            ((==) pipeline)
                            -- TODO this lambda could be a utility/helper in the Concourse module
                            (\pipeline -> { pipeline | paused = not pipeline.paused })
                            pipelines
                in
                    ( model
                    , Cmd.none
                    )

            PipelinePauseToggled _ (Err _) ->
                ( model, Cmd.none )

            GroupMsg (Group.DragStart teamName index) ->
                case model.state of
                    HasData substate ->
                        ( { model | state = HasData { substate | dragState = Group.Dragging teamName index } }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            GroupMsg (Group.DragOver teamName index) ->
                case model.state of
                    HasData substate ->
                        ( { model | state = HasData { substate | dropState = Group.Dropping index } }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            GroupMsg (Group.PipelineMsg msg) ->
                flip update model <| PipelineMsg msg

            PipelineMsg (Pipeline.Tooltip pipelineName teamName) ->
                ( model, tooltip ( pipelineName, teamName ) )

            -- TODO this case is also too long. hopefully some of it can be pulled into utility functions,
            -- or in general be moved into the Group module
            GroupMsg Group.DragEnd ->
                case model.state of
                    HasData substate ->
                        -- TODO can we do this part of the context with a lens? some kind of Optional for
                        -- whether you are dragging at all...
                        case ( substate.dragState, substate.dropState ) of
                            ( Group.Dragging teamName dragIndex, Group.Dropping dropIndex ) ->
                                let
                                    toMaybe : Model -> Maybe SubState
                                    toMaybe m =
                                        case m.state of
                                            HasData substate ->
                                                Just substate

                                            _ ->
                                                Nothing

                                    substateOptional =
                                        Monocle.Optional.Optional (toMaybe) (\s m -> { m | state = HasData s })

                                    liftMaybe : Monocle.Lens.Lens a b -> Monocle.Lens.Lens (Maybe a) (Maybe b)
                                    liftMaybe l =
                                        Monocle.Lens.Lens (Maybe.map l.get) (Maybe.map2 l.set)

                                    dragStateLens =
                                        Monocle.Lens.Lens .dragState (\ds ss -> { ss | dragState = ds })

                                    dropStateLens =
                                        Monocle.Lens.Lens .dropState (\ds ss -> { ss | dropState = ds })

                                    teamDataLens =
                                        Monocle.Lens.Lens .teamData (\td ss -> { ss | teamData = td })

                                    setApiData : Group.APIData -> TeamData -> TeamData
                                    setApiData apiData teamData =
                                        case teamData of
                                            Unauthenticated _ ->
                                                Unauthenticated { apiData = apiData }

                                            Authenticated { user } ->
                                                Authenticated { apiData = apiData, user = user }

                                    apiDataLens =
                                        Monocle.Lens.Lens teamApiData setApiData

                                    groupsLens =
                                        Monocle.Lens.fromIso <| Monocle.Iso.Iso Group.groups Group.apiData

                                    findGroupOptional =
                                        let
                                            predicate =
                                                .teamName >> (==) teamName
                                        in
                                            Monocle.Optional.Optional (List.Extra.find predicate)
                                                (\g gs -> List.Extra.findIndex predicate gs |> Maybe.map (\i -> List.Extra.setAt i g gs) |> Maybe.Extra.join |> Maybe.withDefault gs)

                                    modifyWithEffect : Monocle.Optional.Optional a b -> (b -> ( b, Cmd msg )) -> a -> ( a, Cmd msg )
                                    modifyWithEffect l f m =
                                        l.getOption m |> Maybe.map f |> Maybe.map (Tuple.mapFirst (flip l.set m)) |> Maybe.withDefault ( m, Cmd.none )

                                    superOptional =
                                        substateOptional
                                            |> flip Monocle.Optional.composeLens teamDataLens
                                            |> flip Monocle.Optional.composeLens apiDataLens
                                            |> flip Monocle.Optional.composeLens groupsLens
                                            |> flip Monocle.Optional.compose findGroupOptional

                                    updatePipelines : Int -> Int -> Group.Group -> ( Group.Group, Cmd Msg )
                                    updatePipelines dragIndex dropIndex group =
                                        let
                                            newGroup =
                                                Group.shiftPipelines dragIndex dropIndex group
                                        in
                                            ( newGroup, orderPipelines newGroup.teamName newGroup.pipelines model.csrfToken )
                                in
                                    model
                                        |> (Monocle.Optional.composeLens substateOptional dragStateLens).set Group.NotDragging
                                        |> (Monocle.Optional.composeLens substateOptional dropStateLens).set Group.NotDropping
                                        |> modifyWithEffect superOptional (updatePipelines dragIndex dropIndex)

                            _ ->
                                ( { model | state = HasData { substate | dragState = Group.NotDragging, dropState = Group.NotDropping } }
                                , Cmd.none
                                )

                    _ ->
                        ( model, Cmd.none )



-- TODO this is pretty hard to reason about. really deeply nested and nasty. doesn't exactly relate
-- to the hd refactor as hd doesn't have the drag-and-drop feature, but it's a big contributor
-- to the 'length of this file' tire fire


shiftPipelineTo : Pipeline.PipelineWithJobs -> Int -> List Pipeline.PipelineWithJobs -> List Pipeline.PipelineWithJobs
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


orderPipelines : String -> List Pipeline.PipelineWithJobs -> Concourse.CSRFToken -> Cmd Msg
orderPipelines teamName pipelines csrfToken =
    Task.attempt (always Noop) <|
        Concourse.Pipeline.order
            teamName
            (List.map (.name << .pipeline) <| pipelines)
            csrfToken



-- TODO this seems obsessed with pipelines. shouldn't be the dashboard's business


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
    let
        mainContent =
            case model.state of
                NotAsked ->
                    Html.text ""

                Turbulence path ->
                    turbulenceView path

                NoPipelines ->
                    Html.map (always Noop) NoPipeline.view

                HasData substate ->
                    pipelinesView substate model.showHelp model.topBar.query
    in
        Html.div
            [ class "dashboard" ]
            [ mainContent
            , helpView model
            ]


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


footerView : SubState -> Bool -> Html Msg
footerView substate showHelp =
    Html.div
        [ if substate.hideFooter || showHelp then
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
                [ Html.text "version: v", substate.teamData |> teamApiData |> .version |> Html.text ]
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


turbulenceView : String -> Html Msg
turbulenceView path =
    Html.div
        [ class "error-message" ]
        [ Html.div [ class "message" ]
            [ Html.img [ src path, class "seatbelt" ] []
            , Html.p [] [ Html.text "experiencing turbulence" ]
            , Html.p [ class "explanation" ] []
            ]
        ]


pipelinesView : SubState -> Bool -> String -> Html Msg
pipelinesView substate showHelp query =
    let
        filteredGroups =
            substate.teamData |> teamApiData |> Group.groups |> filter query

        groupsToDisplay =
            if List.all (String.startsWith "team:") (filterTerms query) then
                filteredGroups
            else
                filteredGroups |> List.filter (.pipelines >> List.isEmpty >> not)

        groupViews =
            case substate.teamData of
                Unauthenticated _ ->
                    List.map (\g -> Group.view (Group.headerView g) substate.dragState substate.dropState substate.now g) groupsToDisplay

                Authenticated { user } ->
                    List.map (\g -> Group.view (GroupWithTag.headerView g) substate.dragState substate.dropState substate.now g.group) (GroupWithTag.addTagsAndSort user groupsToDisplay)
    in
        if List.isEmpty groupViews then
            noResultsView (toString query)
        else
            Html.div [ class "dashboard-content" ] <|
                -- TODO woops, this is non SRP/spaghetti. clearly the pipelines shouldn't care about the help,
                -- and from an HTML semantics perspective they shouldn't be in the same container
                (List.map (Html.map GroupMsg) groupViews ++ [ footerView substate showHelp ])


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
    Group.remoteData
        |> Task.andThen remoteUser
        |> Task.map2 (,) Time.now
        |> RemoteData.asCmd
        |> Cmd.map APIDataFetched


remoteUser : Group.APIData -> Task.Task Http.Error ( Group.APIData, Maybe Concourse.User )
remoteUser d =
    Concourse.User.fetchUser
        |> Task.map ((,) d << Just)
        |> Task.onError (always <| Task.succeed <| ( d, Nothing ))


getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform ClockTick Time.now


filterTerms : String -> List String
filterTerms =
    replace All (regex "team:\\s*") (\_ -> "team:")
        >> replace All (regex "status:\\s*") (\_ -> "status:")
        >> String.words
        >> List.filter (not << String.isEmpty)


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
