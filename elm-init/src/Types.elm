module Types exposing (..)

import Http
import Bytes exposing (Bytes)
import File exposing (File)

type Msg
  = ChangeMode Mode
  | ChangeTon String
  | Input String
  | ChangeTheory String
  | DownloadTheory
  | Reset

type Mode = Major | Minor

type alias Model =
  { mode : Mode
  , tonality : Int
  , previous : Int
  , current : Int
  , next : Int
  , output : List Chord
  , theory : Int
  }

major : Chord -> List Int
major s =
  case s of
    I -> [0, 4, 7]
    II -> [2, 5, 9]
    IIN -> [1, 5, 8]
    IV -> [5, 9, 0]
    V -> [7, 11, 2, 5]
    V9 -> [7, 11, 5, 8]
    VI -> [9, 0, 4]
    VII -> [11, 2, 5, 8]
    V_II -> [9, 1, 4, 7]
    VII_II -> [1, 4, 7, 10]
    V_IV -> [0, 4, 7, 10]
    VII_IV -> [4, 7, 10, 1]
    V_V -> [2, 6, 9, 0]
    VII_V -> [6, 9, 0, 3]
    V_VI -> [4, 8, 11, 2]
    VII_VI -> [8, 11, 2, 5]
    _ -> []

minor : Chord -> List Int
minor s =
  case s of
    I -> [0, 3, 7]
    II -> [2, 5, 8]
    IIN -> [1, 5, 8]
    IV -> [5, 8, 0]
    V -> [7, 11, 2, 5]
    V9 -> [7, 11, 5, 8]
    VI -> [8, 0, 3]
    VII -> [11, 2, 5, 8]
    V_IV -> [0, 4, 7, 10]
    VII_IV -> [4, 7, 10, 1]
    V_V -> [2, 6, 9, 0]
    VII_V -> [6, 9, 0, 3]
    V_VI -> [4, 8, 11, 2]
    VII_VI -> [8, 11, 2, 5]
    _ -> []

toChord : String -> Chord
toChord s =
  case s of
    "I" -> I
    "II" -> II
    "IIN" -> IIN
    "IV" -> IV
    "V" -> V
    "V9" -> V9
    "VI" -> VI
    "VII" -> VII
    "V_II" -> V_II
    "VII_II" -> VII_II
    "V_IV" -> V_IV
    "VII_IV" -> VII_IV
    "V_V" -> V_V
    "VII_V" -> VII_V
    "V_VI" -> V_VI
    "VII_VI" -> VII_VI
    _ -> ERROR

fromChord : Chord -> String
fromChord s =
  case s of
    I -> "I"
    II -> "II"
    IIN -> "IIN"
    IV -> "IV"
    V -> "V"
    V9 -> "V9"
    VI -> "VI"
    VII -> "VII"
    V_II -> "V_II"
    VII_II -> "VII_II"
    V_IV -> "V_IV"
    VII_IV -> "VII_IV"
    V_V -> "V_V"
    VII_V -> "VII_V"
    V_VI -> "V_VI"
    VII_VI -> "VII_VI"
    _ -> ""


majorChords : List Chord
majorChords=
  [ I
  , II
  , IIN
  , IV
  , V
  , V9
  , VI
  , VII
  , V_II
  , VII_II
  , V_IV
  , VII_IV
  , V_V
  , VII_V
  , V_VI
  , VII_VI
  ]

minorChords : List Chord
minorChords =
  [ I
  , II
  , IIN
  , IV
  , V
  , V9
  , VI
  , VII
  , V_IV
  , VII_IV
  , V_V
  , VII_V
  , V_VI
  , VII_VI
  ]

type Chord
 = I
 | II
 | IIN
 | IV
 | V
 | V9
 | VI
 | VII
 | V_II
 | VII_II
 | V_IV
 | VII_IV
 | V_V
 | VII_V
 | V_VI
 | VII_VI
 | ERROR
