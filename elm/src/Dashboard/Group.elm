module Dashboard.Group exposing (Msg(..), DragState(..), DropState(..), APIData, Group, groups, apiData, ordering, remoteData, view, pipelineDropAreaView, headerView, shiftPipelines)

import Concourse
import Concourse.Info
import Concourse.Job
import Concourse.Pipeline
import Concourse.PipelineStatus
import Concourse.Resource
import Concourse.Team
import Dashboard.Pipeline as Pipeline
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Http
import Json.Decode
import Ordering exposing (Ordering)
import Set
import Task
import Time exposing (Time)


type alias Group =
    { pipelines : List Pipeline.PipelineWithJobs
    , teamName : String
    }


type alias APIData =
    { teams : List Concourse.Team
    , pipelines : List Concourse.Pipeline
    , jobs : List Concourse.Job
    , resources : List Concourse.Resource
    , version : String
    }


type alias PipelineIndex =
    Int


type DragState
    = NotDragging
    | Dragging Concourse.TeamName PipelineIndex


type DropState
    = NotDropping
    | Dropping PipelineIndex


type Msg
    = DragStart String Int
    | DragOver String Int
    | DragEnd
    | PipelineMsg Pipeline.Msg


allPipelines : APIData -> List Pipeline.PipelineWithJobs
allPipelines data =
    data.pipelines
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


shiftPipelines : Int -> Int -> Group -> Group
shiftPipelines dragIndex dropIndex group =
    if dragIndex == dropIndex then
        group
    else
        let
            pipelines =
                case
                    List.head <|
                        List.drop dragIndex <|
                            group.pipelines
                of
                    Nothing ->
                        group.pipelines

                    Just pipeline ->
                        shiftPipelineTo pipeline dropIndex group.pipelines
        in
            { group | pipelines = pipelines }



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


allTeamNames : APIData -> List String
allTeamNames apiData =
    Set.union
        (Set.fromList (List.map .teamName apiData.pipelines))
        (Set.fromList (List.map .name apiData.teams))
        |> Set.toList


remoteData : Task.Task Http.Error APIData
remoteData =
    Task.map5 APIData
        Concourse.Team.fetchTeams
        Concourse.Pipeline.fetchPipelines
        (Concourse.Job.fetchAllJobs |> Task.map (Maybe.withDefault []))
        (Concourse.Resource.fetchAllResources |> Task.map (Maybe.withDefault []))
        (Concourse.Info.fetch |> Task.map .version)


groups : APIData -> List Group
groups apiData =
    let
        teamNames =
            allTeamNames apiData
    in
        teamNames
            |> List.map (group (allPipelines apiData))



-- TODO i'd like for this to be an isomorphism, which would
-- require adding resource data to the Group type, or making
-- the APIData type smaller (or, like, not marrying Group to
-- APIData at all but using a different type)


apiData : List Group -> APIData
apiData groups =
    let
        pipelines =
            groups |> List.concatMap .pipelines
    in
        { teams = groups |> List.map (\g -> { id = 0, name = g.teamName })
        , pipelines = pipelines |> List.map .pipeline
        , jobs = pipelines |> List.concatMap .jobs
        , resources = []
        , version = ""
        }


group : List Pipeline.PipelineWithJobs -> String -> Group
group allPipelines teamName =
    { pipelines = (List.filter ((==) teamName << .teamName << .pipeline) allPipelines)
    , teamName = teamName
    }


ordering : Ordering Group
ordering =
    Ordering.byField .teamName


view : List (Html Msg) -> DragState -> DropState -> Time -> Group -> Html Msg
view header dragState dropState now group =
    let
        pipelines =
            if List.isEmpty group.pipelines then
                [ Pipeline.pipelineNotSetView ]
            else
                List.append
                    (List.indexedMap
                        (\i pipeline ->
                            Html.div [ class "pipeline-wrapper" ]
                                [ pipelineDropAreaView dragState dropState group.teamName i
                                , Html.div
                                    [ classList
                                        [ ( "dashboard-pipeline", True )
                                        , ( "dashboard-paused", pipeline.pipeline.paused )
                                        , ( "dashboard-running", not <| List.isEmpty <| List.filterMap .nextBuild pipeline.jobs )
                                        , ( "dashboard-status-" ++ Concourse.PipelineStatus.show (Pipeline.pipelineStatusFromJobs pipeline.jobs False), not pipeline.pipeline.paused )
                                        , ( "dragging", dragState == Dragging pipeline.pipeline.teamName i )
                                        ]
                                    , attribute "data-pipeline-name" pipeline.pipeline.name
                                    , attribute "ondragstart" "event.dataTransfer.setData('text/plain', '');"
                                    , draggable "true"
                                    , on "dragstart" (Json.Decode.succeed (DragStart pipeline.pipeline.teamName i))
                                    , on "dragend" (Json.Decode.succeed DragEnd)
                                    ]
                                    [ Html.div [ class "dashboard-pipeline-banner" ] []
                                    , Html.map PipelineMsg <| Pipeline.pipelineView now pipeline i
                                    ]
                                ]
                        )
                        group.pipelines
                    )
                    [ pipelineDropAreaView dragState dropState group.teamName (List.length group.pipelines) ]
    in
        Html.div [ id group.teamName, class "dashboard-team-group", attribute "data-team-name" group.teamName ]
            [ Html.div [ class "pin-wrapper" ]
                [ Html.div [ class "dashboard-team-header" ] header ]
            , Html.div [ class "dashboard-team-pipelines" ] pipelines
            ]


headerView : Group -> List (Html Msg)
headerView group =
    [ Html.div [ class "dashboard-team-name" ] [ Html.text group.teamName ] ]


pipelineDropAreaView : DragState -> DropState -> String -> Int -> Html Msg
pipelineDropAreaView dragState dropState teamName index =
    let
        ( active, over ) =
            case ( dragState, dropState ) of
                ( Dragging team dragIndex, NotDropping ) ->
                    ( team == teamName, index == dragIndex )

                ( Dragging team dragIndex, Dropping dropIndex ) ->
                    ( team == teamName, index == dropIndex )

                _ ->
                    ( False, False )
    in
        Html.div
            [ classList [ ( "drop-area", True ), ( "active", active ), ( "over", over ), ( "animation", dropState /= NotDropping ) ]
            , on "dragenter" (Json.Decode.succeed (DragOver teamName index))
            ]
            [ Html.text "" ]
