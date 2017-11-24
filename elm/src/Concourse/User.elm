module Concourse.User exposing (fetchUser, logOut)

import Http
import HttpBuilder
import Task exposing (Task)
import Concourse


fetchUser : Task Http.Error Concourse.User
fetchUser =
    HttpBuilder.get
        (Concourse.host
            ++ "/api/v1/user"
        )
        |> HttpBuilder.withCredentials
        |> HttpBuilder.withExpect (Http.expectJson Concourse.decodeUser)
        |> HttpBuilder.toTask


logOut : Task Http.Error ()
logOut =
    HttpBuilder.get
        (Concourse.host
            ++ "/auth/logout"
        )
        |> HttpBuilder.withCredentials
        |> HttpBuilder.withExpect (Http.expectStringResponse (\_ -> Ok ()))
        |> HttpBuilder.toTask
