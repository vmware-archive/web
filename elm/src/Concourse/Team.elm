module Concourse.Team exposing (fetchTeams)

import Concourse
import Http
import Json.Decode
import Task exposing (Task)


fetchTeams : Task Http.Error (List Concourse.Team)
fetchTeams =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url = Concourse.host ++ "/api/v1/teams"
            , expect = Http.expectJson (Json.Decode.list Concourse.decodeTeam)
            , timeout = Nothing
            , withCredentials = True
            , headers = []
            , body = Http.emptyBody
            }
