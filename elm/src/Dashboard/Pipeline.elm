module Dashboard.Pipeline exposing (Msg(..), DragState(..), DropState(..), pipelineNotSetView, pipelineDropAreaView, pipelineView, pipelineStatus)

import BuildDuration
import Concourse
import Concourse.PipelineStatus
import DashboardHelpers exposing (..)
import DashboardPreview
import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onMouseEnter)
import Json.Decode
import Routes
import StrictEvents exposing (onLeftClick)
import Time exposing (Time)


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
    | Tooltip String String
    | TogglePipelinePaused Concourse.Pipeline


pipelineNotSetView : Html msg
pipelineNotSetView =
    Html.div [ class "pipeline-wrapper" ]
        [ Html.div
            [ class "dashboard-pipeline no-set"
            ]
            [ Html.div
                [ class "dashboard-pipeline-content" ]
                [ Html.div [ class "no-set-wrapper" ]
                    [ Html.text "no pipelines set" ]
                ]
            ]
        ]


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


pipelineView : DragState -> Maybe Time -> PipelineWithJobs -> Int -> Html Msg
pipelineView dragState now ({ pipeline, jobs, resourceError } as pipelineWithJobs) index =
    Html.div
        [ classList
            [ ( "dashboard-pipeline", True )
            , ( "dashboard-paused", pipeline.paused )
            , ( "dashboard-running", List.any (\job -> job.nextBuild /= Nothing) jobs )
            , ( "dashboard-status-" ++ Concourse.PipelineStatus.show (pipelineStatusFromJobs jobs False), not pipeline.paused )
            , ( "dragging", dragState == Dragging pipeline.teamName index )
            ]
        , attribute "data-pipeline-name" pipeline.name
        , attribute "ondragstart" "event.dataTransfer.setData('text/plain', '');"
        , draggable "true"
        , on "dragstart" (Json.Decode.succeed (DragStart pipeline.teamName index))
        , on "dragend" (Json.Decode.succeed DragEnd)
        ]
        [ Html.div [ class "dashboard-pipeline-banner" ] []
        , Html.div
            [ class "dashboard-pipeline-content" ]
            [ Html.a [ href <| Routes.pipelineRoute pipeline, draggable "false" ]
                [ Html.div
                    [ class "dashboard-pipeline-header"
                    , onMouseEnter <| Tooltip pipeline.name pipeline.teamName
                    ]
                    [ Html.div [ class "dashboard-pipeline-name" ]
                        [ Html.text pipeline.name ]
                    , Html.div [ classList [ ( "dashboard-resource-error", resourceError ) ] ] []
                    ]
                ]
            , DashboardPreview.view jobs
            , Html.div [ class "dashboard-pipeline-footer" ]
                [ Html.div [ class "dashboard-pipeline-icon" ] []
                , timeSincePipelineTransitioned now pipelineWithJobs
                , pauseToggleView pipeline
                ]
            ]
        ]


timeSincePipelineTransitioned : Maybe Time -> PipelineWithJobs -> Html a
timeSincePipelineTransitioned time ({ jobs } as pipelineWithJobs) =
    let
        status =
            pipelineStatus pipelineWithJobs

        transitionedJobs =
            List.filter
                (\job ->
                    not <| xor (status == Concourse.PipelineStatusSucceeded) (Just Concourse.BuildStatusSucceeded == Maybe.map .status job.finishedBuild)
                )
                jobs

        transitionedDurations =
            List.filterMap
                (\job ->
                    Maybe.map .duration job.transitionBuild
                )
                transitionedJobs

        sortedTransitionedDurations =
            List.sortBy
                (\duration ->
                    case duration.startedAt of
                        Just date ->
                            Time.inSeconds <| Date.toTime date

                        Nothing ->
                            0
                )
                transitionedDurations

        transitionedDuration =
            if status == Concourse.PipelineStatusSucceeded then
                List.head << List.reverse <| sortedTransitionedDurations
            else
                List.head <| sortedTransitionedDurations
    in
        case status of
            Concourse.PipelineStatusPaused ->
                Html.div [ class "build-duration" ] [ Html.text "paused" ]

            Concourse.PipelineStatusPending ->
                Html.div [ class "build-duration" ] [ Html.text "pending" ]

            Concourse.PipelineStatusRunning ->
                Html.div [ class "build-duration" ] [ Html.text "running" ]

            _ ->
                case ( time, transitionedDuration ) of
                    ( Just now, Just duration ) ->
                        BuildDuration.show duration now

                    _ ->
                        Html.text ""


pipelineStatus : PipelineWithJobs -> Concourse.PipelineStatus
pipelineStatus { pipeline, jobs } =
    if pipeline.paused then
        Concourse.PipelineStatusPaused
    else
        pipelineStatusFromJobs jobs True


pauseToggleView : Concourse.Pipeline -> Html Msg
pauseToggleView pipeline =
    Html.a
        [ classList
            [ ( "pause-toggle", True )
            , ( "icon-play", pipeline.paused )
            , ( "icon-pause", not pipeline.paused )
            ]
        , onLeftClick <| TogglePipelinePaused pipeline
        ]
        []
