module BuildTests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Build exposing (..)
import Concourse.Pagination
import Concourse
import Dict
import RemoteData


all : Test
all =
    describe "Navigating the build history"
        [ test "updates the history of the build page" <|
            \_ ->
                let
                    ( defaultModel, _ ) =
                        Build.init
                            { title = (\_ -> Cmd.none) }
                            { csrfToken = "", hash = "" }
                            (BuildPage 1)

                    content =
                        [ { id = 15
                          , url = "/teams/main/pipelines/states/jobs/passing/builds/7"
                          , name = "7"
                          , job = Just { teamName = "main", pipelineName = "states", jobName = "passing" }
                          , status = Concourse.BuildStatusPending
                          , duration = { startedAt = Nothing, finishedAt = Nothing }
                          , reapTime = Nothing
                          }
                        ]

                    pagination =
                        { previousPage = Nothing
                        , nextPage =
                            Just
                                { direction = Concourse.Pagination.Since 18
                                , limit = 100
                                }
                        }

                    history =
                        { content = content
                        , pagination = pagination
                        }

                    currentBuild =
                        RemoteData.Success
                            { build =
                                { id = 15
                                , url = "/teams/main/pipelines/states/jobs/passing/builds/7"
                                , name = "7"
                                , job = Just { teamName = "main", pipelineName = "states", jobName = "passing" }
                                , status = Concourse.BuildStatusPending
                                , duration = { startedAt = Nothing, finishedAt = Nothing }
                                , reapTime = Nothing
                                }
                            , prep =
                                Just
                                    { pausedPipeline = Concourse.BuildPrepStatusNotBlocking
                                    , pausedJob = Concourse.BuildPrepStatusNotBlocking
                                    , maxRunningBuilds = Concourse.BuildPrepStatusNotBlocking
                                    , inputs = Dict.fromList []
                                    , inputsSatisfied = Concourse.BuildPrepStatusNotBlocking
                                    , missingInputReasons = Dict.fromList []
                                    }
                            , output = Nothing
                            }

                    initModel =
                        { defaultModel | currentBuild = currentBuild }

                    expectedModel =
                        { initModel
                            | history = content
                            , pagination = pagination
                        }

                    ( actualModel, _ ) =
                        Build.update (BuildHistoryFetched (Ok history)) initModel
                in
                    Expect.equal actualModel expectedModel
        ]
