module SpacePreview exposing (Model, init, view, update, Msg(..))

import Concourse
import Concourse.BuildStatus
import Debug
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, classList, href, tabindex)
import Html.Events exposing (onClick, onMouseOver, onMouseLeave, onWithOptions)
import SpaceRoutes
import Json.Decode as Json


type alias Model =
    ( String, String )


type Msg
    = ResourceHoverMsg ( String, String )
    | NavTo String


init : Model
init =
    ( "", "" )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResourceHoverMsg resourceSpace ->
            ( resourceSpace, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : List Concourse.SpaceJob -> List Concourse.SpaceResource -> Model -> Html Msg
view jobs resources resourceSpace =
    let
        groups =
            jobGroups jobs

        resourcesDict =
            List.foldl
                (\resource byName ->
                    Dict.insert resource.name resource byName
                )
                Dict.empty
                resources
    in
        Html.div [ class "pipeline-grid" ] <|
            List.map
                (\jobs ->
                    List.map (viewJob resourceSpace resourcesDict) jobs
                        |> Html.div [ class "space-parallel-grid" ]
                )
                (Dict.values groups)


viewJob : ( String, String ) -> Dict String Concourse.SpaceResource -> Concourse.SpaceJob -> Html Msg
viewJob resourceSpace resources job =
    Html.div [ class "node" ]
        [ Html.div
            [ classList [ ( "job", True ), ( "paused", job.paused ) ] ]
            [ Html.a [ class "job-name" ]
                [ Html.text job.name
                ]
            , Html.div [ class "combination-container" ] <|
                List.map (\combination -> viewJobCombination combination resourceSpace resources job) job.combintations
            ]
        ]


jobCombinationLink : Concourse.SpaceJobCombination -> String
jobCombinationLink jobCombination =
    case ( jobCombination.finishedBuild, jobCombination.nextBuild ) of
        ( _, Just nb ) ->
            SpaceRoutes.buildRoute nb

        ( Just fb, Nothing ) ->
            SpaceRoutes.buildRoute fb

        ( Nothing, Nothing ) ->
            ""


viewJobCombination : Concourse.SpaceJobCombination -> ( String, String ) -> Dict String Concourse.SpaceResource -> Concourse.SpaceJob -> Html Msg
viewJobCombination jobCombination ( resource, space ) resources job =
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
            [ classList
                [ ( "job-combination", True )
                , ( buildStatus, True )
                , ( "inactive", not active )
                ]
            , onClick <| NavTo <| jobCombinationLink jobCombination
            ]
            [ Html.div
                [ class "job-combination-resources"
                , tabindex 0
                , onWithOptions "click" { stopPropagation = True, preventDefault = True } (Json.succeed <| NavTo "")
                ]
                []
            , Html.div
                [ class "job-combination-details" ]
                (jobCombinationPopover resources job jobCombination)
            ]


jobCombinationPopover : Dict String Concourse.SpaceResource -> Concourse.SpaceJob -> Concourse.SpaceJobCombination -> List (Html Msg)
jobCombinationPopover resources job jobCombination =
    let
        space =
            \name combination ->
                let
                    spaceName =
                        Dict.get name combination
                in
                    case spaceName of
                        Just "default" ->
                            name ++ "\n"

                        Just sp ->
                            name ++ " [" ++ (Maybe.withDefault "" <| Dict.get name combination) ++ "] \n"

                        _ ->
                            name ++ "\n"

        inputs =
            List.map
                (\input ->
                    let
                        resourceStatus =
                            case Dict.get input.resource resources of
                                Just resource ->
                                    case ( resource.paused, resource.checkError ) of
                                        ( True, _ ) ->
                                            "paused"

                                        ( False, "" ) ->
                                            ""

                                        _ ->
                                            "errored"

                                _ ->
                                    ""
                    in
                        Html.li
                            [ class <| "job-combination-input " ++ resourceStatus
                            , onMouseOver (ResourceHoverMsg ( input.resource, Maybe.withDefault "default" <| Dict.get input.resource jobCombination.combination ))
                            , onMouseLeave (ResourceHoverMsg ( "", "" ))
                            ]
                            [ Html.span [ classList [ ( "trigger", input.trigger ) ] ] []
                            , Html.span [ class "name" ] [ Html.text <| space input.resource jobCombination.combination ]
                            ]
                )
                job.inputs

        outputs =
            List.map
                (\output ->
                    let
                        resourceStatus =
                            case Dict.get output.resource resources of
                                Just resource ->
                                    case ( resource.paused, resource.checkError ) of
                                        ( True, _ ) ->
                                            "paused"

                                        ( False, "" ) ->
                                            ""

                                        _ ->
                                            "errored"

                                _ ->
                                    ""
                    in
                        Html.li
                            [ class <| "job-combination-output " ++ resourceStatus
                            , onMouseOver (ResourceHoverMsg ( output.resource, Maybe.withDefault "default" <| Dict.get output.resource jobCombination.combination ))
                            , onMouseLeave (ResourceHoverMsg ( "", "" ))
                            ]
                            [ Html.span [ class "name" ] [ Html.text <| space output.resource jobCombination.combination ]
                            ]
                )
                job.outputs
    in
        [ Html.ul [ class "job-combination-inputs" ] <|
            [ Html.li []
                [ Html.span [ class "description" ] [ Html.text "get " ]
                , Html.i [ class "fa fa-fw fa-arrow-down" ] []
                ]
            ]
                ++ inputs
        , Html.ul [ class "job-combination-outputs" ] <|
            [ Html.li []
                [ Html.span [ class "description" ] [ Html.text "put " ]
                , Html.i [ class "fa fa-fw fa-arrow-up" ] []
                ]
            ]
                ++ outputs
        ]


jobGroups : List Concourse.SpaceJob -> Dict Int (List Concourse.SpaceJob)
jobGroups jobs =
    let
        jobLookup =
            jobByName <| List.foldl (\job byName -> Dict.insert job.name job byName) Dict.empty jobs
    in
        Dict.foldl
            (\jobName depth byDepth ->
                Dict.update depth
                    (\jobsA ->
                        Just (jobLookup jobName :: Maybe.withDefault [] jobsA)
                    )
                    byDepth
            )
            Dict.empty
            (jobDepths jobs Dict.empty)


jobByName : Dict String Concourse.SpaceJob -> String -> Concourse.SpaceJob
jobByName jobs job =
    case Dict.get job jobs of
        Just a ->
            a

        Nothing ->
            Debug.crash "impossible"


jobDepths : List Concourse.SpaceJob -> Dict String Int -> Dict String Int
jobDepths jobs dict =
    case jobs of
        [] ->
            dict

        job :: otherJobs ->
            let
                passedJobs =
                    List.concatMap .passed job.inputs
            in
                case List.length passedJobs of
                    0 ->
                        jobDepths otherJobs <| Dict.insert job.name 0 dict

                    _ ->
                        let
                            passedJobDepths =
                                List.map (\passedJob -> Dict.get passedJob dict) passedJobs
                        in
                            if List.member Nothing passedJobDepths then
                                jobDepths (List.append otherJobs [ job ]) dict
                            else
                                let
                                    depths =
                                        List.map (\depth -> Maybe.withDefault 0 depth) passedJobDepths

                                    maxPassedJobDepth =
                                        Maybe.withDefault 0 <| List.maximum depths
                                in
                                    jobDepths otherJobs <| Dict.insert job.name (maxPassedJobDepth + 1) dict
