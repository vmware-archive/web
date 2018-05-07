module SpaceRoutes exposing (ConcourseRoute, Route(..), parsePath, navigateTo, pipelineRoute, buildRoute, toString, customToString)

import Concourse
import Concourse.Pagination as Pagination
import Navigation exposing (Location)
import QueryString
import Route exposing (..)


type Route
    = SpaceHome
    | SpacePipeline String String
    | SpaceBuild String String String String String


type alias ConcourseRoute =
    { logical : Route
    , queries : QueryString.QueryString
    , page : Maybe Pagination.Page
    , hash : String
    }



-- pages


spacePipeline : Route.Route Route
spacePipeline =
    SpacePipeline := static "space" </> static "teams" </> string </> static "pipelines" </> string


spaceBuild : Route.Route Route
spaceBuild =
    SpaceBuild := static "space" </> static "teams" </> string </> static "pipelines" </> string </> static "jobs" </> string </> string </> static "builds" </> string



-- route utils


pipelineRoute : Concourse.Pipeline -> String
pipelineRoute p =
    (SpacePipeline p.teamName p.name) |> toString


buildRoute : Concourse.Build -> String
buildRoute b =
    case b.job of
        Just j ->
            SpaceBuild j.teamName j.pipelineName j.jobName (Basics.toString b.jobCombinationID) b.name |> toString

        Nothing ->
            SpaceHome |> toString



-- router


sitemap : Router Route
sitemap =
    router
        [ spacePipeline
        , spaceBuild
        ]


match : String -> Route
match =
    Route.match sitemap
        >> Maybe.withDefault SpaceHome


toString : Route -> String
toString route =
    case route of
        SpacePipeline teamName pipelineName ->
            reverse spacePipeline [ teamName, pipelineName ]

        SpaceBuild teamName pipelineName jobName jobCombinationID buildID ->
            reverse spaceBuild [ teamName, pipelineName, jobName, jobCombinationID, buildID ]

        SpaceHome ->
            "/space"


parsePath : Location -> ConcourseRoute
parsePath location =
    { logical = match <| location.pathname
    , queries = QueryString.parse location.search |> QueryString.remove "csrf_token"
    , page = createPageFromSearch location.search
    , hash = location.hash
    }


customToString : ConcourseRoute -> String
customToString route =
    toString route.logical ++ QueryString.render route.queries


createPageFromSearch : String -> Maybe Pagination.Page
createPageFromSearch search =
    let
        q =
            QueryString.parse search

        until =
            QueryString.one QueryString.int "until" q

        since =
            QueryString.one QueryString.int "since" q

        limit =
            Maybe.withDefault 100 <| QueryString.one QueryString.int "limit" q
    in
        case ( since, until ) of
            ( Nothing, Just u ) ->
                Just
                    { direction = Pagination.Until u
                    , limit = limit
                    }

            ( Just s, Nothing ) ->
                Just
                    { direction = Pagination.Since s
                    , limit = limit
                    }

            _ ->
                Nothing


navigateTo : Route -> Cmd msg
navigateTo =
    toString >> Navigation.newUrl
