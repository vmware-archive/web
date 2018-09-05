module Dashboard.Group exposing (APIData, Grouped, groups, ordering, remoteData, view)

import Concourse
import Concourse.Info
import Concourse.Job
import Concourse.Pipeline
import Concourse.Resource
import Concourse.Team
import DashboardHelpers exposing (PipelineWithJobs)
import Dashboard.Pipeline as Pipeline
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Ordering exposing (Ordering)
import Set
import Task
import Time exposing (Time)


type alias Grouped a =
    { a
        | pipelines : List PipelineWithJobs
        , teamName : String
    }


type alias APIData =
    { teams : List Concourse.Team
    , pipelines : List Concourse.Pipeline
    , jobs : List Concourse.Job
    , resources : List Concourse.Resource
    , version : String
    }


allPipelines : APIData -> List PipelineWithJobs
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


groups : APIData -> List (Grouped {})
groups apiData =
    (List.map << group)
        (allPipelines apiData)
        (allTeamNames apiData)


group : List PipelineWithJobs -> String -> Grouped {}
group allPipelines teamName =
    { pipelines = (List.filter ((==) teamName << .teamName << .pipeline) allPipelines)
    , teamName = teamName
    }


ordering : Ordering (Grouped a)
ordering =
    Ordering.byField .teamName


view : Pipeline.DragState -> Pipeline.DropState -> Maybe Time -> Grouped a -> Html Pipeline.Msg
view dragState dropState now group =
    let
        pipelines =
            if List.isEmpty group.pipelines then
                [ Pipeline.pipelineNotSetView ]
            else
                List.append
                    (List.indexedMap
                        (\i pipeline ->
                            Html.div [ class "pipeline-wrapper" ] [ Pipeline.pipelineDropAreaView dragState dropState group.teamName i, Pipeline.pipelineView dragState now pipeline i ]
                        )
                        group.pipelines
                    )
                    [ Pipeline.pipelineDropAreaView dragState dropState group.teamName (List.length group.pipelines) ]
    in
        Html.div [ id group.teamName, class "dashboard-team-group", attribute "data-team-name" group.teamName ]
            [ Html.div [ class "pin-wrapper" ] <|
                [ Html.div [ class "dashboard-team-name" ] [ Html.text group.teamName ]
                ]
            , Html.div [ class "dashboard-team-pipelines" ] pipelines
            ]
