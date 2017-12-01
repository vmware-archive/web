module Concourse.Request exposing (get)

import Http
import Task exposing (Task)
import Concourse


get : String -> Http.Expect a -> Task Http.Error a
get uri expect =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url = Concourse.host ++ uri
            , expect = expect
            , timeout = Nothing
            , withCredentials = False
            , headers = []
            , body = Http.emptyBody
            }
