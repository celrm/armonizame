module View exposing (view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)

title : Html Msg
title =
  div
    [ style "font-size" "30px"
    , style "margin" "50px 0px"
    ]
    [ text "Basic Harmony Helper"]

tonality : Html Msg
tonality =
  div [style "column-count" "2"]
    [ text " Tonality ", br [] []
    , select [ onInput ChangeTon ]
        (List.range 0 11 |> List.map noteToOption)
    , br [style "margin-bottom" "30px"] []
    , label []
        [ input
            [ type_ "radio"
            , name "mode"
            , checked True
            , onClick (ChangeMode Major)
            ]
            []
        , text " Major "
        ]
    , br [] []
    , label []
        [ input
            [ type_ "radio"
            , name "mode"
            , checked False
            , onClick (ChangeMode Minor)
            ]
            []
        , text " Minor"
        ]
    ]

selection : Model -> Html Msg
selection model =
  div [style "column-count" "3"]
  [ text "Previous", br [] []
  , span [style "border" "1px solid black", style "padding" "0px 10px"] [text (toSNote model.previous)], br [] []
  , text "Current", br [] []
  , span [style "border" "1px solid black", style "padding" "0px 10px"] [text (toSNote model.current)], br [] []
  , text "Next", br [] []
  , select [ onInput Input ]
      (List.range -1 11 |> List.map noteToOption), br [] []
  ]

algorithm : Model -> List Chord
algorithm model =
  let note =
        if model.current == -1
          then -1
          else modBy 12 (model.current - model.tonality + 12)
    in
  case model.mode of
    Major ->
      majorChords
      |> List.filterMap (\c -> if List.member note (major c) then Just c else Nothing)
    Minor ->
      minorChords
      |> List.filterMap (\c -> if List.member note (minor c) then Just c else Nothing)

lessons : Html Msg
lessons =
  div []
    [ text "Lesson"
    , select [ onInput ChangeTheory, style "margin" "0px 20px"  ]
      (List.range 1 16 |> List.map intToOption)
    , button [onClick DownloadTheory] [ text "Download theory" ]
    ]

view : Model -> Browser.Document Msg
view model =
  (Browser.Document "Basic Harmony Helper"
    [ div [style "text-align" "center"]
      [ title
      , tonality
      , br [] []
      , selection model
      , br [] []
      , (algorithm model)
        |> List.map (\c -> p [] [text (fromChord c)] )
        |> (\l -> div [] l)
      , button [onClick Reset] [text "Reset"]
      , br [] [], br [] []
      , lessons
      ]
    ]
  )

toNote : Int -> String
toNote k =
  case k of
    0 -> "C"
    1 -> "C#"
    2 -> "D"
    3 -> "Eb"
    4 -> "E"
    5 -> "F"
    6 -> "F#"
    7 -> "G"
    8 -> "G#"
    9 -> "A"
    10 -> "Bb"
    11 -> "B"
    _ -> " "

toSNote : Int -> String
toSNote k =
  case k of
    0 -> "Do"
    1 -> "Do#"
    2 -> "Re"
    3 -> "Reb"
    4 -> "Mi"
    5 -> "Fa"
    6 -> "Fa#"
    7 -> "Sol"
    8 -> "Sol#"
    9 -> "La"
    10 -> "Sib"
    11 -> "Si"
    _ -> " "

noteToOption : Int -> Html Msg
noteToOption v =
  option [ value (String.fromInt v) ] [ text (toSNote v) ]

intToOption : Int -> Html Msg
intToOption v =
  option [ value (String.fromInt v) ] [ text (String.fromInt v) ]
