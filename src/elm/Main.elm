module Main exposing (main)

import Html.App
import Cart.State exposing (init, update, subscriptions)
import Cart.View exposing (view)


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
