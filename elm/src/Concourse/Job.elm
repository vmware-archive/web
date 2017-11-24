-- TODO: explicit exposing


module Concourse.Job exposing (..)

import Http
import HttpBuilder
import Task exposing (Task)
import Json.Decode
import Concourse


fetchJob : Concourse.JobIdentifier -> Task Http.Error Concourse.Job
fetchJob job =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url = Concourse.host ++ "/api/v1/teams/" ++ job.teamName ++ "/pipelines/" ++ job.pipelineName ++ "/jobs/" ++ job.jobName
            , expect = Http.expectJson (Concourse.decodeJob { teamName = job.teamName, pipelineName = job.pipelineName })
            , timeout = Nothing
            , withCredentials = True
            , headers = []
            , body = Http.emptyBody
            }


fetchJobs : Concourse.PipelineIdentifier -> Task Http.Error (List Concourse.Job)
fetchJobs pi =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url = Concourse.host ++ "/api/v1/teams/" ++ pi.teamName ++ "/pipelines/" ++ pi.pipelineName ++ "/jobs"
            , expect = Http.expectJson (Json.Decode.list (Concourse.decodeJob pi))
            , timeout = Nothing
            , withCredentials = True
            , headers = []
            , body = Http.emptyBody
            }


fetchJobsWithTransitionBuilds : Concourse.PipelineIdentifier -> Task Http.Error (List Concourse.Job)
fetchJobsWithTransitionBuilds pi =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url = Concourse.host ++ "/api/v1/teams/" ++ pi.teamName ++ "/pipelines/" ++ pi.pipelineName ++ "/jobs" ++ "?include=transitionBuilds"
            , expect = Http.expectJson (Json.Decode.list (Concourse.decodeJob pi))
            , timeout = Nothing
            , withCredentials = True
            , headers = []
            , body = Http.emptyBody
            }


fetchJobsRaw : Concourse.PipelineIdentifier -> Task Http.Error Json.Decode.Value
fetchJobsRaw pi =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url = Concourse.host ++ "/api/v1/teams/" ++ pi.teamName ++ "/pipelines/" ++ pi.pipelineName ++ "/jobs"
            , expect = Http.expectJson Json.Decode.value
            , timeout = Nothing
            , withCredentials = True
            , headers = []
            , body = Http.emptyBody
            }


triggerBuild : Concourse.JobIdentifier -> Concourse.CSRFToken -> Task Http.Error Concourse.Build
triggerBuild job csrfToken =
    HttpBuilder.post (Concourse.host ++ "/api/v1/teams/" ++ job.teamName ++ "/pipelines/" ++ job.pipelineName ++ "/jobs/" ++ job.jobName ++ "/builds")
        |> HttpBuilder.withCredentials
        |> HttpBuilder.withHeader Concourse.csrfTokenHeaderName csrfToken
        |> HttpBuilder.withExpect (Http.expectJson (Concourse.decodeBuild))
        |> HttpBuilder.toTask


pause : Concourse.JobIdentifier -> Concourse.CSRFToken -> Task Http.Error ()
pause =
    pauseUnpause True


unpause : Concourse.JobIdentifier -> Concourse.CSRFToken -> Task Http.Error ()
unpause =
    pauseUnpause False


pauseUnpause : Bool -> Concourse.JobIdentifier -> Concourse.CSRFToken -> Task Http.Error ()
pauseUnpause pause { teamName, pipelineName, jobName } csrfToken =
    let
        action =
            if pause then
                "pause"
            else
                "unpause"
    in
        Http.toTask <|
            Http.request
                { method = "PUT"
                , url = Concourse.host ++ "/api/v1/teams/" ++ teamName ++ "/pipelines/" ++ pipelineName ++ "/jobs/" ++ jobName ++ "/" ++ action
                , headers = [ Http.header Concourse.csrfTokenHeaderName csrfToken ]
                , body = Http.emptyBody
                , expect = Http.expectStringResponse (\_ -> Ok ())
                , timeout = Nothing
                , withCredentials = True
                }
