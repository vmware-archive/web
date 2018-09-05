module Dashboard.GroupWithTag exposing (Authed, addTags, taggedGroups)

import Concourse
import Dashboard.Group as Group
import Dashboard.Group.Tag as Tag
import Ordering exposing (Ordering)


type alias Tagged a =
    { a | tag : Tag.Tag }


type alias Authed a =
    { a | user : Concourse.User }


taggedGroups : Authed Group.APIData -> List (Tagged (Group.Grouped {}))
taggedGroups authed =
    Group.groups
        { teams = authed.teams
        , pipelines = authed.pipelines
        , jobs = authed.jobs
        , resources = authed.resources
        , version = authed.version
        }
        |> addTags authed.user


addTags : Concourse.User -> List (Group.Grouped {}) -> List (Tagged (Group.Grouped {}))
addTags user =
    List.map
        (\{ pipelines, teamName } ->
            { pipelines = pipelines
            , teamName = teamName
            , tag = Tag.tag user teamName
            }
        )


ordering : Ordering (Tagged (Group.Grouped a))
ordering =
    Ordering.byFieldWith Tag.ordering .tag
        |> Ordering.breakTiesWith Group.ordering
