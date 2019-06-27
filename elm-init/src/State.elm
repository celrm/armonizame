module State exposing (init, subscriptions, update)

import Types exposing (..)
import File.Download
import Json.Decode exposing (Decoder, field, list)
import Http
import File exposing (File)

init : () -> (Model, Cmd Msg)
init _ =
  ( { mode = Major
    , tonality = 0
    , previous = -1
    , current = -1
    , next = -1
    , output = []
    , theory = 0
    }
  , Cmd.none
  )

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

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      ChangeMode m ->
        ( { model | mode = m }, Cmd.none )
      ChangeTon s ->
        ( { model |
            tonality = Maybe.withDefault 0 (String.toInt s)
          }, Cmd.none )
      Input s ->
        ( { model
            | previous = model.current
            , current = model.next
            , next = Maybe.withDefault 0 (String.toInt s)
          }, Cmd.none )
      ChangeTheory s ->
        let k = Maybe.withDefault 1 (String.toInt s) in
        ( { model | theory = k }, Cmd.none )
      DownloadTheory ->
        ( model, File.Download.url (theoryUrl model.theory))
      Reset ->
        ( { model
            | previous = -1
            , current = -1
            , next = -1
            , output = []
          }, Cmd.none )

dataDecoder : Decoder File
dataDecoder = File.decoder

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
    Sub.none
