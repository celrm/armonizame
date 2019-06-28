module Types exposing (..)

import Stack exposing (Stack)

type Msg
  = ChangeMode Mode
  | ChangeTon String
  | Input String
  | UpdateOutput ()
  | ChangeTheory String
  | DownloadTheory
  | Reset
  | Previous
  | Next
  | KeyDown String
  -- | KeyUp

type Mode = Major | Minor

type alias Model =
  { mode : Mode
  , tonic : Int
  , previous : Stack Int
  , current : Int
  -- , new : Int
  , next : Stack Int
  , output : List Chord
  , theory : Int
  -- , key : String
  }


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
