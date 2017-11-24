module Concourse.Info
    exposing
        ( fetch
        )

import Concourse
import Http
import Task exposing (Task)


fetch : Task Http.Error Concourse.Info
fetch =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url = Concourse.host ++ "/api/v1/info"
            , expect = Http.expectJson Concourse.decodeInfo
            , timeout = Nothing
            , withCredentials = True
            , headers = []
            , body = Http.emptyBody
            }
