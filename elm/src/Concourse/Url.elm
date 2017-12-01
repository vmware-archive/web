module Concourse.Url exposing (url)

import Concourse
import Routes


url : Concourse.Foo -> String
url foo =
    case foo of
        Concourse.Build ->
            Routes.toString <| Routes.Build foo.teamName foo.pipelineName foo.jobName foo.name

        Concourse.Job ->
            Routes.toString <| Routes.Job foo.teamName foo.pipelineName foo.name

        Concourse.Resource ->
            Routes.toString <| Routes.Resource foo.teamName foo.pipelineName foo.name
