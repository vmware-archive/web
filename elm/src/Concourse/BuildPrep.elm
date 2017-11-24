module Concourse.BuildPrep exposing (..)

import Http
import Task exposing (Task)
import Concourse


fetch : Concourse.BuildId -> Task Http.Error Concourse.BuildPrep
fetch buildId =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url = Concourse.host ++ "/api/v1/builds/" ++ toString buildId ++ "/preparation"
            , expect = Http.expectJson Concourse.decodeBuildPrep
            , timeout = Nothing
            , withCredentials = True
            , headers = []
            , body = Http.emptyBody
            }
