module Main exposing (main)

import Browser
import Types exposing (..)
import State exposing (init, update, subscriptions)
import View exposing (view)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
