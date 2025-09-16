module Main exposing (main)

import Browser
import Html exposing (div, text)
import Html.Attributes exposing (class)


main =
    Browser.sandbox
        { init = ()
        , update = \_ model -> model
        , view = \_ -> text ""
        }
