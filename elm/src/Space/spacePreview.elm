module SpacePreview exposing (view)

import Concourse
import Concourse.BuildStatus
import Debug
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, classList, href)
import SpaceRoutes


view : List Concourse.SpaceJob -> ( String, String ) -> Html msg
view jobs resourceSpace =
    let
        groups =
            jobGroups jobs
    in
        Html.div [ class "pipeline-grid" ] <|
            List.map
                (\jobs ->
                    List.map (viewJob resourceSpace) jobs
                        |> Html.div [ class "space-parallel-grid" ]
                )
                (Dict.values groups)


viewJob : ( String, String ) -> Concourse.SpaceJob -> Html msg
viewJob resourceSpace job =
    Html.div [ class "node" ]
        [ Html.div
            [ classList [ ( "job", True ), ( "paused", job.paused ) ] ]
            [ Html.a [ class "job-name" ]
                [ Html.text job.name
                ]
            , Html.div [ class "combination-container" ] <|
                List.map (\combination -> viewJobCombination combination resourceSpace job) job.combintations
            ]
        ]


viewJobCombination : Concourse.SpaceJobCombination -> ( String, String ) -> Concourse.SpaceJob -> Html msg
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
                    , ( buildStatus, True )
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
