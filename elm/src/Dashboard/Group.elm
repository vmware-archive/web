module Dashboard.Group exposing (Group, Data, groups, groupOrdering, view)

import Concourse
import DashboardHelpers exposing (PipelineWithJobs)
import Dashboard.Pipeline as Pipeline
import Html exposing (..)
import Html.Attributes exposing (..)
import Ordering exposing (Ordering)
import Time exposing (Time)


type alias Group =
    { pipelines : List PipelineWithJobs
    , team : Concourse.Team
    }


type alias Data =
    { allPipelines : List PipelineWithJobs
    , teams : List Concourse.Team
    }


groups : Data -> List Group
groups data =
    List.map (group data.allPipelines) data.teams


group : List PipelineWithJobs -> Concourse.Team -> Group
group allPipelines team =
    Group (List.filter ((==) team.name << .teamName << .pipeline) allPipelines) team


groupOrdering : Ordering Group
groupOrdering =
    Ordering.byField (.team >> .name)


view : Pipeline.DragState -> Pipeline.DropState -> Maybe Time -> Group -> Html Pipeline.Msg
view dragState dropState now group =
    let
        pipelines =
            if List.isEmpty group.pipelines then
                [ Pipeline.pipelineNotSetView ]
            else
                List.append
                    (List.indexedMap
                        (\i pipeline ->
                            Html.div [ class "pipeline-wrapper" ] [ Pipeline.pipelineDropAreaView dragState dropState group.team.name i, Pipeline.pipelineView dragState now pipeline i ]
                        )
                        group.pipelines
                    )
                    [ Pipeline.pipelineDropAreaView dragState dropState group.team.name (List.length group.pipelines) ]
    in
        Html.div [ id group.team.name, class "dashboard-team-group", attribute "data-team-name" group.team.name ]
            [ Html.div [ class "pin-wrapper" ] <|
                [ Html.div [ class "dashboard-team-name" ] [ Html.text group.team.name ]
                ]
            , Html.div [ class "dashboard-team-group.pipelines" ] pipelines
            ]
