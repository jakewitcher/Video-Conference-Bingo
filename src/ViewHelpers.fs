[<AutoOpen>]
module ViewHelpers

open Feliz

let div (classes: string list) (children: ReactElement list) =
  Html.div [
    prop.className classes
    prop.children children
  ]