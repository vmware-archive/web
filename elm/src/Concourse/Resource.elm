module Concourse.Resource
    exposing
        ( fetchResource
        , fetchResourcesRaw
        , pause
        , unpause
        , fetchVersionedResources
        , fetchVersionedResource
        , enableVersionedResource
        , disableVersionedResource
        , fetchInputTo
        , fetchOutputOf
        , fetchCausality
        )

import Concourse
import Concourse.Pagination exposing (Pagination, Paginated, Page)
import Http
import Json.Decode
import Task exposing (Task)


fetchResource : Concourse.ResourceIdentifier -> Task Http.Error Concourse.Resource
fetchResource rid =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url =
                Concourse.host
                    ++ "/api/v1/teams/"
                    ++ rid.teamName
                    ++ "/pipelines/"
                    ++ rid.pipelineName
                    ++ "/resources/"
                    ++ rid.resourceName
            , expect = Http.expectJson Concourse.decodeResource
            , timeout = Nothing
            , withCredentials = True
            , headers = []
            , body = Http.emptyBody
            }


fetchResourcesRaw : Concourse.PipelineIdentifier -> Task Http.Error Json.Decode.Value
fetchResourcesRaw pi =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url = Concourse.host ++ "/api/v1/teams/" ++ pi.teamName ++ "/pipelines/" ++ pi.pipelineName ++ "/resources"
            , expect = Http.expectJson Json.Decode.value
            , timeout = Nothing
            , withCredentials = True
            , headers = []
            , body = Http.emptyBody
            }


pause : Concourse.ResourceIdentifier -> Concourse.CSRFToken -> Task Http.Error ()
pause =
    pauseUnpause True


unpause : Concourse.ResourceIdentifier -> Concourse.CSRFToken -> Task Http.Error ()
unpause =
    pauseUnpause False


pauseUnpause : Bool -> Concourse.ResourceIdentifier -> Concourse.CSRFToken -> Task Http.Error ()
pauseUnpause pause rid csrfToken =
    let
        action =
            if pause then
                "pause"
            else
                "unpause"
    in
        Http.toTask <|
            Http.request
                { method = "PUT"
                , url = Concourse.host ++ "/api/v1/teams/" ++ rid.teamName ++ "/pipelines/" ++ rid.pipelineName ++ "/resources/" ++ rid.resourceName ++ "/" ++ action
                , headers = [ Http.header Concourse.csrfTokenHeaderName csrfToken ]
                , body = Http.emptyBody
                , expect = Http.expectStringResponse (\_ -> Ok ())
                , timeout = Nothing
                , withCredentials = True
                }


fetchVersionedResource : Concourse.VersionedResourceIdentifier -> Task Http.Error Concourse.VersionedResource
fetchVersionedResource vrid =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url =
                Concourse.host
                    ++ "/api/v1/teams/"
                    ++ vrid.teamName
                    ++ "/pipelines/"
                    ++ vrid.pipelineName
                    ++ "/resources/"
                    ++ vrid.resourceName
                    ++ "/versions/"
                    ++ toString vrid.versionID
            , expect = Http.expectJson Concourse.decodeVersionedResource
            , timeout = Nothing
            , withCredentials = True
            , headers = []
            , body = Http.emptyBody
            }


fetchVersionedResources : Concourse.ResourceIdentifier -> Maybe Page -> Task Http.Error (Paginated Concourse.VersionedResource)
fetchVersionedResources rid page =
    let
        url =
            Concourse.host ++ "/api/v1/teams/" ++ rid.teamName ++ "/pipelines/" ++ rid.pipelineName ++ "/resources/" ++ rid.resourceName ++ "/versions"
    in
        Concourse.Pagination.fetch Concourse.decodeVersionedResource url page


enableVersionedResource : Concourse.VersionedResourceIdentifier -> Concourse.CSRFToken -> Task Http.Error ()
enableVersionedResource =
    enableDisableVersionedResource True


disableVersionedResource : Concourse.VersionedResourceIdentifier -> Concourse.CSRFToken -> Task Http.Error ()
disableVersionedResource =
    enableDisableVersionedResource False


enableDisableVersionedResource : Bool -> Concourse.VersionedResourceIdentifier -> Concourse.CSRFToken -> Task Http.Error ()
enableDisableVersionedResource enable vrid csrfToken =
    let
        action =
            if enable then
                "enable"
            else
                "disable"
    in
        Http.toTask <|
            Http.request
                { method = "PUT"
                , url = Concourse.host ++ "/api/v1/teams/" ++ vrid.teamName ++ "/pipelines/" ++ vrid.pipelineName ++ "/resources/" ++ vrid.resourceName ++ "/versions/" ++ (toString vrid.versionID) ++ "/" ++ action
                , headers = [ Http.header Concourse.csrfTokenHeaderName csrfToken ]
                , body = Http.emptyBody
                , expect = Http.expectStringResponse (\_ -> Ok ())
                , timeout = Nothing
                , withCredentials = True
                }


fetchInputTo : Concourse.VersionedResourceIdentifier -> Task Http.Error (List Concourse.Build)
fetchInputTo =
    fetchInputOutput "input_to"


fetchOutputOf : Concourse.VersionedResourceIdentifier -> Task Http.Error (List Concourse.Build)
fetchOutputOf =
    fetchInputOutput "output_of"


fetchInputOutput : String -> Concourse.VersionedResourceIdentifier -> Task Http.Error (List Concourse.Build)
fetchInputOutput action vrid =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url =
                Concourse.host
                    ++ "/api/v1/teams/"
                    ++ vrid.teamName
                    ++ "/pipelines/"
                    ++ vrid.pipelineName
                    ++ "/resources/"
                    ++ vrid.resourceName
                    ++ "/versions/"
                    ++ toString vrid.versionID
                    ++ "/"
                    ++ action
            , expect = Http.expectJson (Json.Decode.list Concourse.decodeBuild)
            , timeout = Nothing
            , withCredentials = True
            , headers = []
            , body = Http.emptyBody
            }


fetchCausality : Concourse.VersionedResourceIdentifier -> Task Http.Error (List Concourse.Cause)
fetchCausality vrid =
    Http.toTask <|
        Http.request
            { method = "GET"
            , url =
                Concourse.host
                    ++ "/api/v1/teams/"
                    ++ vrid.teamName
                    ++ "/pipelines/"
                    ++ vrid.pipelineName
                    ++ "/resources/"
                    ++ vrid.resourceName
                    ++ "/versions/"
                    ++ toString vrid.versionID
                    ++ "/causality"
            , expect = Http.expectJson (Json.Decode.list Concourse.decodeCause)
            , timeout = Nothing
            , withCredentials = True
            , headers = []
            , body = Http.emptyBody
            }
