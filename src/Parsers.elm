module Parsers exposing (..)

import Types exposing (..)

import Stack exposing (Stack)

top : Stack Int -> Int
top s = Maybe.withDefault 0 (Stack.top s)

maybeToInt : String -> Int
maybeToInt s = Maybe.withDefault 0 (String.toInt s)

fromkey : String -> String
fromkey s =
  case s of
    "a" -> "0"
    "A" -> "0"
    "w" -> "1"
    "W" -> "1"
    "s" -> "2"
    "S" -> "2"
    "e" -> "3"
    "E" -> "3"
    "d" -> "4"
    "D" -> "4"
    "f" -> "5"
    "F" -> "5"
    "t" -> "6"
    "T" -> "6"
    "g" -> "7"
    "G" -> "7"
    "y" -> "8"
    "Y" -> "8"
    "h" -> "9"
    "H" -> "9"
    "u" -> "10"
    "U" -> "10"
    "j" -> "11"
    "J" -> "11"
    otherwise -> ""

tokey : Int -> String
tokey a =
  case a of
    0 -> "a"
    1 -> "w"
    2 -> "s"
    3 -> "e"
    4 -> "d"
    5 -> "f"
    6 -> "t"
    7 -> "g"
    8 -> "y"
    9 -> "h"
    10 -> "u"
    11 -> "j"
    otherwise -> ""

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
    V_II -> "V/II"
    VII_II -> "VII/II"
    V_IV -> "V/IV"
    VII_IV -> "VII/IV"
    V_V -> "V/V"
    VII_V -> "VII/V"
    V_VI -> "V/VI"
    VII_VI -> "VII/VI"
    _ -> ""


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
    1 -> "Reb"
    2 -> "Re"
    3 -> "Mib"
    4 -> "Mi"
    5 -> "Fa"
    6 -> "Fa#"
    7 -> "Sol"
    8 -> "Sol#"
    9 -> "La"
    10 -> "Sib"
    11 -> "Si"
    _ -> " "
