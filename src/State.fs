module State

type Status = Selected | Unselected | FreeSpace

type Column = A | B | C | D | E

type Row = One | Two | Three | Four | Five

type Position =
    { Column: Column 
      Row: Row }

type Description = string

type Tile = 
    { Position: Position
      Description: Description 
      Status: Status }

type BingoCard = Tile list

type State =
    { BingoCard: BingoCard
      WinConditionMet: bool }

type Msg =
    | ToggleSelection of Position
    | ResetBingoCard