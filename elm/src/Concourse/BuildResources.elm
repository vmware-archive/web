module Concourse.BuildResources exposing (empty, fetch)

import Http
import Task exposing (Task)
import Concourse


empty : Concourse.BuildResources
empty =
    { inputs = []
    , outputs = []
    }


fetch : Concourse.BuildId -> Task Http.Error Concourse.BuildResources
fetch buildId =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url = Concourse.host ++ "/api/v1/builds/" ++ toString buildId ++ "/resources"
            , expect = Http.expectJson Concourse.decodeBuildResources
            , timeout = Nothing
            , withCredentials = True
            , headers = []
            , body = Http.emptyBody
            }
