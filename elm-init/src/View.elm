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
    [ text "Asistente Armónico"]

tonality : Html Msg
tonality =
  div [style "column-count" "2"]
    [ text " Tonalidad ", br [] []
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
        , text " Mayor "
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
        , text " Menor"
        ]
    ]

selection : Model -> Html Msg
selection model =
  div [style "column-count" "3"]
  [ text "Anterior", br [] []
  , span [style "border" "1px solid black", style "padding" "0px 10px"] [text (toSNote model.previous)], br [] []
  , text "Actual", br [] []
  , span [style "border" "1px solid black", style "padding" "0px 10px"] [text (toSNote model.current)], br [] []
  , text "Siguiente", br [] []
  , select [ onInput Input ]
      (List.range -1 11 |> List.map noteToOption), br [] []
  ]

lessons : Html Msg
lessons =
  div []
    [ text "Lección"
    , select [ onInput ChangeTheory, style "margin" "0px 20px"  ]
      (List.range 1 16 |> List.map intToOption)
    , button [onClick DownloadTheory] [ text "Descargar teoría" ]
    ]

view : Model -> Browser.Document Msg
view model =
  (Browser.Document "Asistente Armónico"
    [ div [style "text-align" "center"]
      [ title
      , tonality
      , br [] []
      , selection model
      , br [] []
      , model.output
        |> List.map fromChord
        |> List.map (\c -> p [] [text c] )
        |> (\l -> div [] l)
      , button [onClick Reset] [text "Reseteo"]
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
