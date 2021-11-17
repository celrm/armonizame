module Credits exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

credits : Html msg
credits =
  div
  [ style "margin-top" "100px"
  , style "left" "0px"
  , style "bottom" "0px"
  , style "position" "fixed"
  , style "background-color" "white"
  , style "font-size" "0.75em"
  ]
  [ span [style "margin-left" "30px"] [text "Celia Rubio Madrigal"]
  , span [style "margin-left" "70px"] [text "Email: "]
  , a [ href "mailto:celrubio@ucm.es"] [text "celrubio@ucm.es"]
  ]
