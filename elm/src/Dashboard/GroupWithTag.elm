module Dashboard.GroupWithTag exposing (GroupWithTag, AuthenticatedContext, groupsWithTags, addTags)

import Concourse
import Dashboard.Group as Group
import Dashboard.Group.Tag as Tag
import Ordering exposing (Ordering)


type alias GroupWithTag =
    { group : Group.Group, tag : Tag.Tag }


type alias AuthenticatedContext =
    { user : Concourse.User
    , data : Group.Data
    }


addTags : Concourse.User -> List Group.Group -> List GroupWithTag
addTags user =
    List.map (\g -> GroupWithTag g <| Tag.tag user g.teamName)


groupsWithTags : AuthenticatedContext -> List GroupWithTag
groupsWithTags context =
    List.sortWith groupOrderingWithTag <| List.map2 GroupWithTag (Group.groups context.data) (tags context)


groupOrderingWithTag : Ordering GroupWithTag
groupOrderingWithTag =
    Ordering.byFieldWith Tag.ordering .tag |> Ordering.breakTiesWith (Ordering.byFieldWith Group.ordering .group)


tags : AuthenticatedContext -> List Tag.Tag
tags context =
    List.map (Tag.tag context.user) context.data.teamNames
