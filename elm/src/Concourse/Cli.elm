module Concourse.Cli exposing (downloadUrl)

import Concourse


downloadUrl : String -> String -> String
downloadUrl arch platform =
    Concourse.host ++ "/api/v1/cli?arch=" ++ arch ++ "&platform=" ++ platform
