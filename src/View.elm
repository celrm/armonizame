module View exposing (view)

import Types exposing (..)
import Parsers exposing (..)
import Credits exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

title : Html Msg
title =
  div
    [ style "font-size" "30px"
    , style "margin" "30px 0px"
    ]
    [ text "Asistente Armónico"]

tonality : Html Msg
tonality =
  div [style "column-count" "2"]
    [ text " Tónica ", br [] []
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

-- selection : Model -> Html Msg
-- selection model =
--   div [style "column-count" "3"]
--   [ text "Anterior", br [] []
--   , span [style "border" "1px solid black", style "padding" "0px 10px"] [text (toSNote <| top model.previous)], br [] []
--   , text "Actual", br [] []
--   , span [style "border" "1px solid black", style "padding" "0px 10px"] [text (toSNote model.current)], br [] []
--   , text "Siguiente", br [] []
--   , span [style "border" "1px solid black", style "padding" "0px 10px"] [text (toSNote <| model.new)], br [] []
--   -- , select [ onInput Input ]
--   --     (List.range -1 11 |> List.map noteToOption), br [] []
--   ]

-- addAlert bool add =
--   (\m ->
--       if bool
--       then add :: m
--       else m
--      )
--
-- alerts : Model -> Html Msg
-- alerts model =
--   let prev = top model.previous in
--   []
--     |> addAlert
--         (model.current /= -1
--         && modBy 12 (model.tonic - model.current + 12) == 1
--         && model.tonic /= model.new)
--         "No has resuelto la sensible"
--     |> toHtml "red"

controls : Model -> Html Msg
controls model =
  div []
  [ button
    [onClick Previous, disabled (top model.previous == -2)]
    [text "↶ Atrás"]
  , button
    [onClick Reset, style "margin" "0px 20px" ]
    [text "Reset"]
  , button
    [onClick Next, disabled (top model.next == -2)]
    [text "Adelante ↷"]
  ]

toHtml : String -> List String -> Html Msg
toHtml col list =
  list
    |> List.map
      (\c -> div
        [style "color" col]
        [text c]
      )
    |> div
      [ style "column-count" "2"
      , style "width" "450px"
      , style "margin" "auto"
      ]

lessons : Html Msg
lessons =
  div []
    [ text "Lección"
    , select
      [ onInput ChangeTheory, style "margin" "0px 20px" ]
      (List.range 1 16 |> List.map intToOption)
    , button
      [onClick DownloadTheory]
      [ text "Descargar teoría" ]
    ]

view : Model -> Browser.Document Msg
view model =
  (Browser.Document "Asistente Armónico"
    [ div [style "text-align" "center", style "width" "750px", style "margin" "auto"]
      [ title
      , tonality, br [] []
      , keyboard (tokey model.current), br [] []
      -- , select [ onInput Input, style "margin-bottom" "20px" ]
      --     (List.range -1 11 |> List.map noteToOption), br [] []
      -- , alerts model, br [] []
      -- , selection model, br [] []
      , controls model, br [] []
      , model.output
        |> List.map fromChord
        |> toHtml "black"
      , br [] []
      , lessons
      , br [] []
      , credits
      ]
    ]
  )

noSelect : List (Attribute Msg)
noSelect =
  [ style "-webkit-touch-callout" "none"
  , style "-webkit-user-select" "none"
  , style "-khtml-user-select" "none"
  , style "-moz-user-select" "none"
  , style "-ms-user-select" "none"
  , style "user-select" "none"
  ]

keyboard : String -> Html Msg
keyboard k =
  div
  [ style "display" "flex"
  , style "width" "600px"
  , style "height" "200px"
  , style "margin" "auto"
  , style "flex-flow" "column"
  ]
  [ div
    [ style "background-color" "#404040"
    , style "height" "10%"
    , style "outline-style" "solid"
    , style "outline-color" "#404040"
    , style "outline-width" "2px"
    ]
    []
  , div
    [ style "display" "flex"
    , style "flex-flow" "row"
    , style "flex" "4"
    ]
    (List.map (key k)
    [ (True,"a"), (False,"w"), (True,"s"), (False,"e"), (True,"d"), (False,""), (True,"f"), (False,"t"), (True,"g"), (False,"y"), (True,"h"), (False,"u"), (True,"j")])
  ]

key : String -> (Bool, String) -> Html Msg
key k (b, s) =
  div
  (noSelect ++ [ style "align-items" "center"
  , style "display" "inline-flex"
  , style "flex" "1"
  , style "padding-bottom" "1%"
  , style "justify-content" "flex-end"
  , style "flex-flow" "column"
  , style "outline" "2px solid #404040"
  , onMouseDown (KeyDown s)
  -- , onMouseUp KeyUp
  ] ++ if b then
    [ style "justify-content" "flex-end"
    , style "color" (if k == s then "white"
    else "#404040")
    , style "background-color" (if k == s then "#808080"
    else "white")
    ]
  else
    ([ style "position" "relative"
    , style "height" "50%"
    , style "background-color" (if k == s then "#808080"
    else "#404040")
    , style "color" "white"
    , style "width" "8%"
    , style "margin" "0 -4%"
    ] ++ (if s /= "" then []
      else [ style "opacity" "0" ])
    )
  )
  [ text s ]

noteToOption : Int -> Html Msg
noteToOption v =
  option [ value (String.fromInt v) ] [ text (toSNote v) ]

intToOption : Int -> Html Msg
intToOption v =
  option [ value (String.fromInt v) ] [ text (String.fromInt v) ]
