module SpacePipeline exposing (Flags, Model, Msg, init, update, view, subscriptions, changeToPipeline)

import Concourse
import Concourse.BuildStatus
import Concourse.Job
import Concourse.Resource
import Dict exposing (Dict)
import Graph exposing (Graph)
import Grid exposing (Grid)
import Html exposing (Html)
import Html.Attributes exposing (class, classList, href, rowspan, attribute)
import Html.Events exposing (onMouseOver, onMouseLeave)
import Http
import SpaceRoutes
import Task
import Time exposing (Time)


type alias Model =
    { ports : Ports
    , pipelineLocator : Concourse.PipelineIdentifier
    , jobs : List Concourse.SpaceJob
    , resources : List Concourse.SpaceResource
    , error : Maybe String
    , turbulenceImgSrc : String
    , selectedResourceSpace : ( String, String )
    }


type alias ByName a =
    Dict String a


type Node
    = JobNode Concourse.SpaceJob
    | InputNode
        { resourceName : String
        , dependentJob : Concourse.SpaceJob
        }
    | OutputNode
        { resourceName : String
        , upstreamJob : Concourse.SpaceJob
        }
    | ConstrainedInputNode
        { resourceName : String
        , dependentJob : Concourse.SpaceJob
        , upstreamJob : Maybe Concourse.SpaceJob
        }


type alias Ports =
    { title : String -> Cmd Msg
    }


type alias Flags =
    { teamName : String
    , pipelineName : String
    , turbulenceImgSrc : String
    , route : SpaceRoutes.ConcourseRoute
    }


type Msg
    = Noop
    | AutoupdateTimerTicked Time
    | JobsFetched (Result Http.Error (List Concourse.SpaceJob))
    | ResourcesFetched (Result Http.Error (List Concourse.SpaceResource))
    | ResourceHoverMsg ( String, String )


init : Ports -> Flags -> ( Model, Cmd Msg )
init ports flags =
    let
        model =
            { ports = ports
            , pipelineLocator =
                { teamName = flags.teamName
                , pipelineName = flags.pipelineName
                }
            , jobs = []
            , resources = []
            , error = Nothing
            , turbulenceImgSrc = flags.turbulenceImgSrc
            , selectedResourceSpace = ( "", "" )
            }
    in
        ( model
        , Cmd.batch
            [ fetchJobs model.pipelineLocator
            , fetchResources model.pipelineLocator
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        AutoupdateTimerTicked timestamp ->
            ( model
            , fetchJobs model.pipelineLocator
            )

        JobsFetched (Ok jobs) ->
            ( { model | jobs = jobs }, Cmd.none )

        JobsFetched (Err msg) ->
            ( { model | error = Just (toString msg) }, Cmd.none )

        ResourcesFetched (Ok resources) ->
            ( { model | resources = resources }, Cmd.none )

        ResourcesFetched (Err msg) ->
            ( { model | error = Just (toString msg) }, Cmd.none )

        ResourceHoverMsg resourceSpace ->
            ( { model | selectedResourceSpace = resourceSpace }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (5 * Time.second) AutoupdateTimerTicked ]


fetchJobs : Concourse.PipelineIdentifier -> Cmd Msg
fetchJobs pid =
    Task.attempt JobsFetched <|
        Concourse.Job.fetchSpaceJobs pid


fetchResources : Concourse.PipelineIdentifier -> Cmd Msg
fetchResources pid =
    Task.attempt ResourcesFetched <|
        Concourse.Resource.fetchSpaceResources pid


changeToPipeline : Flags -> Model -> ( Model, Cmd Msg )
changeToPipeline flags model =
    let
        pid =
            { teamName = flags.teamName
            , pipelineName = flags.pipelineName
            }
    in
        if model.pipelineLocator == pid then
            ( model, Cmd.none )
        else
            init model.ports flags


view : Model -> Html Msg
view model =
    Html.div [ class "pipeline-content" ]
        [ viewResources model.resources
        , Html.div [ class "pipeline-grid" ]
            [ viewGrid (Grid.fromGraph <| initGraph model.jobs) model.selectedResourceSpace ]
        ]


viewResources : List Concourse.SpaceResource -> Html Msg
viewResources resources =
    Html.div [ class "resources" ] <| List.map (\resource -> viewResource resource) resources


viewResource : Concourse.SpaceResource -> Html Msg
viewResource resource =
    Html.div [ class "resource" ]
        [ Html.div [ class "resource-name" ] [ Html.text resource.name ]
        , Html.div [ class "resource-spaces" ] <| List.map (\space -> viewResourceSpace ( resource.name, space )) resource.spaces
        ]


viewResourceSpace : ( String, String ) -> Html Msg
viewResourceSpace ( resource, space ) =
    Html.div
        [ class "resource-space", onMouseOver (ResourceHoverMsg ( resource, space )), onMouseLeave (ResourceHoverMsg ( "", "" )) ]
        [ Html.text space ]


viewGrid : Grid Node () -> ( String, String ) -> Html Msg
viewGrid grid resourceSpace =
    case grid of
        Grid.Cell { node } ->
            viewNode node resourceSpace

        Grid.Serial prev next ->
            Html.div [ class "serial-grid" ]
                (viewSerial prev resourceSpace ++ viewSerial next resourceSpace)

        Grid.Parallel grids ->
            Html.div [ class "space-parallel-grid" ] <|
                List.map (\grid -> viewGrid grid resourceSpace) grids

        Grid.End ->
            Html.text ""


viewSerial : Grid Node () -> ( String, String ) -> List (Html Msg)
viewSerial grid resourceSpace =
    case grid of
        Grid.Serial prev next ->
            viewSerial prev resourceSpace ++ viewSerial next resourceSpace

        _ ->
            [ viewGrid grid resourceSpace ]


viewNode : Graph.Node Node -> ( String, String ) -> Html Msg
viewNode { id, label } resourceSpace =
    let
        idAttr =
            Html.Attributes.id ("node-" ++ toString id)
    in
        case label of
            JobNode job ->
                Html.div [ class "node", idAttr ]
                    [ Html.div
                        [ classList [ ( "job", True ), ( "paused", job.paused ) ] ]
                        [ Html.a [ class "job-name" ]
                            [ Html.text job.name
                            ]
                        , Html.div [ class "combination-container" ] <|
                            List.map (\combination -> viewJobCombination combination resourceSpace job) job.combintations
                        ]
                    ]

            InputNode { resourceName } ->
                Html.div [ class "node input", idAttr ] [ Html.text "" ]

            ConstrainedInputNode { resourceName } ->
                Html.div [ class "node input constrained", idAttr ] [ Html.text "" ]

            OutputNode { resourceName } ->
                Html.div [ class "node output", idAttr ] [ Html.text "" ]


viewJobCombination : Concourse.SpaceJobCombination -> ( String, String ) -> Concourse.SpaceJob -> Html Msg
viewJobCombination jobCombination ( resource, space ) job =
    let
        buildStatus =
            case ( job.paused, jobCombination.finishedBuild, jobCombination.nextBuild ) of
                ( True, _, _ ) ->
                    "paused"

                ( False, Nothing, Nothing ) ->
                    "no-builds"

                ( False, Just build, Nothing ) ->
                    Concourse.BuildStatus.show build.status

                ( False, Nothing, Just build ) ->
                    "no-builds started"

                ( False, Just fb, Just nb ) ->
                    Concourse.BuildStatus.show fb.status ++ " started"

        active =
            (String.isEmpty space) || Dict.get resource jobCombination.combination == Just space
    in
        Html.div
            [ class "job-combination", attribute "data-tooltip" (jobCombinationTooltip job jobCombination) ]
            [ Html.a
                [ classList
                    [ ( "job-combination-status", True )
                    , ( buildStatus, True ) -- FIXME: check for next build
                    , ( "inactive", not active )
                    ]
                , href <| viewJobCombinationLink jobCombination
                ]
                []
            ]


viewJobCombinationLink : Concourse.SpaceJobCombination -> String
viewJobCombinationLink jobCombination =
    case ( jobCombination.finishedBuild, jobCombination.nextBuild ) of
        ( _, Just nb ) ->
            SpaceRoutes.buildRoute nb

        ( Just fb, Nothing ) ->
            SpaceRoutes.buildRoute fb

        ( Nothing, Nothing ) ->
            "/"


jobCombinationTooltip : Concourse.SpaceJob -> Concourse.SpaceJobCombination -> String
jobCombinationTooltip job jobCombination =
    let
        space =
            \name combination -> name ++ " [" ++ (Maybe.withDefault "" <| Dict.get name combination) ++ "] \n"

        inputs =
            List.map (\input -> "⬇ " ++ (space input.name jobCombination.combination)) job.inputs

        outputs =
            List.map (\output -> "⬆ " ++ (space output.name jobCombination.combination)) job.outputs
    in
        String.concat << List.concat <| [ inputs, outputs ]


initGraph : List Concourse.SpaceJob -> Graph Node ()
initGraph jobs =
    let
        jobNodes =
            List.map JobNode jobs

        jobsByName =
            List.foldl (\job dict -> Dict.insert job.name job dict) Dict.empty jobs

        resourceNodes =
            List.concatMap (jobResourceNodes jobsByName) jobs

        graphNodes =
            List.indexedMap Graph.Node (List.concat [ jobNodes, resourceNodes ])
    in
        Graph.fromNodesAndEdges
            graphNodes
            (List.concatMap (nodeEdges graphNodes) graphNodes)


jobResourceNodes : ByName Concourse.SpaceJob -> Concourse.SpaceJob -> List Node
jobResourceNodes jobs job =
    List.concatMap (inputNodes jobs job) job.inputs
        ++ List.concatMap (outputNodes job) job.outputs


inputNodes : ByName Concourse.SpaceJob -> Concourse.SpaceJob -> Concourse.JobInput -> List Node
inputNodes jobs job { resource, passed } =
    if List.isEmpty passed then
        [ InputNode { resourceName = resource, dependentJob = job } ]
    else
        List.map (constrainedInputNode jobs resource job) passed


outputNodes : Concourse.SpaceJob -> Concourse.JobOutput -> List Node
outputNodes job { resource } =
    [ OutputNode { resourceName = resource, upstreamJob = job } ]


constrainedInputNode : ByName Concourse.SpaceJob -> String -> Concourse.SpaceJob -> String -> Node
constrainedInputNode jobs resourceName dependentJob upstreamJobName =
    ConstrainedInputNode
        { resourceName = resourceName
        , dependentJob = dependentJob
        , upstreamJob = Dict.get upstreamJobName jobs
        }


nodeEdges : List (Graph.Node Node) -> Graph.Node Node -> List (Graph.Edge ())
nodeEdges allNodes { id, label } =
    case label of
        JobNode _ ->
            []

        InputNode { dependentJob } ->
            [ Graph.Edge id (jobId allNodes dependentJob) () ]

        ConstrainedInputNode { dependentJob, upstreamJob } ->
            Graph.Edge id (jobId allNodes dependentJob) ()
                :: case upstreamJob of
                    Just upstream ->
                        [ Graph.Edge (jobId allNodes upstream) id () ]

                    Nothing ->
                        []

        OutputNode { upstreamJob } ->
            [ Graph.Edge (jobId allNodes upstreamJob) id () ]


jobId : List (Graph.Node Node) -> Concourse.SpaceJob -> Int
jobId nodes job =
    case List.filter ((==) (JobNode job) << .label) nodes of
        { id } :: _ ->
            id

        [] ->
            Debug.crash "impossible: job index not found"
