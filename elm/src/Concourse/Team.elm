module Concourse.Team exposing (fetchTeams)

import Concourse
import Concourse.Request
import Json.Decode
import Task exposing (Task)
import Http


fetchTeams : Task Http.Error (List Concourse.Team)
fetchTeams =
    Concourse.Request.get "/api/v1/teams" <| Http.expectJson (Json.Decode.list Concourse.decodeTeam)
