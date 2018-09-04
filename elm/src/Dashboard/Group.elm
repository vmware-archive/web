module Dashboard.Group exposing (Group, Data, groups, ordering, view)

import DashboardHelpers exposing (PipelineWithJobs)
import Dashboard.Pipeline as Pipeline
import Html exposing (..)
import Html.Attributes exposing (..)
import Ordering exposing (Ordering)
import Time exposing (Time)


type alias Group =
    { pipelines : List PipelineWithJobs
    , teamName : String
    }


type alias Data =
    { allPipelines : List PipelineWithJobs
    , teamNames : List String
    }


groups : Data -> List Group
groups data =
    List.map (group data.allPipelines) data.teamNames


group : List PipelineWithJobs -> String -> Group
group allPipelines teamName =
    Group (List.filter ((==) teamName << .teamName << .pipeline) allPipelines) teamName


ordering : Ordering Group
ordering =
    Ordering.byField .teamName


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
