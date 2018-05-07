port module SpaceMain exposing (main)

import SpaceLayout
import Navigation


main : Program SpaceLayout.Flags SpaceLayout.Model SpaceLayout.Msg
main =
    Navigation.programWithFlags SpaceLayout.locationMsg
        { init = SpaceLayout.init
        , update = SpaceLayout.update
        , view = SpaceLayout.view
        , subscriptions = SpaceLayout.subscriptions
        }
