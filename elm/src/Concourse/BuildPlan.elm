module Concourse.BuildPlan exposing (..)

import Http
import Task exposing (Task)
import Concourse


fetch : Concourse.BuildId -> Task Http.Error Concourse.BuildPlan
fetch buildId =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url = Concourse.host ++ "/api/v1/builds/" ++ toString buildId ++ "/plan"
            , expect = Http.expectJson Concourse.decodeBuildPlan
            , timeout = Nothing
            , withCredentials = True
            , headers = []
            , body = Http.emptyBody
            }
