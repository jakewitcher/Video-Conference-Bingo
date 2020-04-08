module App

open Elmish
open Elmish.React
open Initialize
open Render
open State
open Update

let init() =
    { BingoCard = generateTiles()
      WinConditionMet = false }

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run