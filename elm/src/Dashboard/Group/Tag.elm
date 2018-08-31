module Dashboard.Group.Tag exposing (..)

import Concourse
import Ordering exposing (Ordering)


type Tag
    = Public
    | Member


ordering : Ordering Tag
ordering =
    Ordering.explicit [ Member, Public ]


text : Tag -> String
text tag =
    case tag of
        Public ->
            "PUBLIC"

        Member ->
            "MEMBER"


tag : Concourse.User -> Concourse.Team -> Tag
tag user team =
    if List.member team.name user.teams then
        Member
    else
        Public
