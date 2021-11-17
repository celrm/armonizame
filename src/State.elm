module State exposing (init, subscriptions, update)

import Types exposing (..)
import Parsers exposing (..)

import File.Download
import File
import Task
import Stack exposing (push, pop, Stack)
import Browser.Events as Keyboard
import Json.Decode as D

init : () -> (Model, Cmd Msg)
init _ =
  ( { mode = Major
    , tonic = 0
    , previous = push -2 Stack.initialise
    , current = -1
    -- , new = -1
    , next = push -2 Stack.initialise
    , output = []
    , theory = 1
    -- , key = ""
    }
  , Cmd.none
  )

algorithm : Model -> List Chord
algorithm model =
  let note =
        if model.current == -1
          then -1
          else modBy 12 (model.current - model.tonic + 12)
    in
  case model.mode of
    Major ->
      majorChords
      |> List.filterMap (\c -> if List.member note (major c) then Just c else Nothing)
    Minor ->
      minorChords
      |> List.filterMap (\c -> if List.member note (minor c) then Just c else Nothing)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      ChangeMode m ->
        ( { model | mode = m }, Cmd.none )
      ChangeTon s ->
        ( { model |
            tonic = maybeToInt s
          }, Cmd.none )
      Input s ->
        ( { model
            | previous = push model.current model.previous
            -- , current = model.new
            -- , new = maybeToInt s
            , current = maybeToInt s
            , next = push -2 Stack.initialise
          }, Task.perform UpdateOutput (Task.succeed ())
        )
      UpdateOutput _ ->
        ( { model
            | output = algorithm model
          }, Cmd.none )
      ChangeTheory s ->
        let k = Maybe.withDefault 1 (String.toInt s) in
        ( { model | theory = k }, Cmd.none )
      DownloadTheory ->
        ( model, File.Download.url (theoryUrl model.theory))
      Reset ->
        ( { model
            | previous = push -2 Stack.initialise
            , current = -1
            -- , new = -1
            , next = push -2 Stack.initialise
          }, Task.perform UpdateOutput (Task.succeed ())
        )
      Previous ->
          ( { model
              | previous = Tuple.second <| pop model.previous
              , current = top model.previous
              -- , new = model.current
              -- , next = push model.new model.next
              , next = push model.current model.next
            }, Task.perform UpdateOutput (Task.succeed ())
          )
      Next ->
        ( { model
              | previous = push model.current model.previous
              -- , current = model.new
              -- , new = top model.next
              , current = top model.next
              , next = Tuple.second <| pop model.next
          }, Task.perform UpdateOutput (Task.succeed ())
        )
      KeyDown k ->
        if String.contains k "awsedftgyhuj"
            then
              ( model, Task.perform Input (Task.succeed (fromkey k)) )
            else
              ( model, Cmd.none )
      -- KeyUp ->
      --   ( {model | key = ""}, Cmd.none )

theoryUrl : Int -> String
theoryUrl k =
  case k of
    1 -> "http://haciendomusica.com/Armonia2/Tema%2001%20-%20Escritura%20para%20coro.pdf"
    2 -> "http://haciendomusica.com/Armonia2/Tema%2002%20-%20El%20enlace%20de%20acordes.pdf"
    3 -> "http://haciendomusica.com/Armonia2/Tema%2003%20-%20El%20Sistema%20Armonico%20Basico.pdf"
    4 -> "http://haciendomusica.com/Armonia2/Tema%2004%20-%201a%20Inversion%20del%20acorde.pdf"
    5 -> "http://haciendomusica.com/Armonia2/Tema%2005%20-%20El%20acorde%20de%207a%20de%20dominante.pdf"
    6 -> "http://haciendomusica.com/Armonia2/Tema%2006%20-%20Las%20Cadencias.pdf"
    7 -> "http://haciendomusica.com/Armonia2/Tema%2007%20-%20Las%20Dominantes%20Secundarias.pdf"
    8 -> "http://haciendomusica.com/Armonia2/Tema%2008%20-%20Las%20Notas%20Extranas%20-%20Introduccion.pdf"
    9 -> "http://haciendomusica.com/Armonia2/Tema%2009%20-%20El%20Modo%20Menor.pdf"
    10 -> "http://haciendomusica.com/Armonia2/Tema%2010%20-%202a%20Inversion%20del%20acorde.pdf"
    11 -> "http://haciendomusica.com/Armonia2/Tema%2011%20-%20Modulacion%20por%20transformacion.pdf"
    12 -> "http://haciendomusica.com/Armonia2/Tema%2012%20-%20El%20V%20grado%20con%209a%20-%20El%20VII%20grado%20con%207a.pdf"
    13 -> "http://haciendomusica.com/Armonia2/Tema%2013%20-%20Progresiones%20y%20series%20de%206as.pdf"
    14 -> "http://haciendomusica.com/Armonia2/Tema%2014%20-%20Las%20Notas%20Extranas%20-%202a%20parte.pdf"
    15 -> "http://haciendomusica.com/Armonia2/Tema%2015%20-%207a%20Diatonica%20y%20Series%20de%207as.pdf"
    16 -> "http://haciendomusica.com/Armonia2/Tema%2016%20-%20Armonia%20Cromatica%20y%20Alterada.pdf"
    _ -> ""

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Keyboard.onKeyDown (D.map KeyDown (D.field "key" D.string))
    -- , Keyboard.onKeyUp (D.succeed KeyUp)
    ]
