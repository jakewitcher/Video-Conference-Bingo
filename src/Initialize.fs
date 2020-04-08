module Initialize

open State
open System

let columns = [ A; B; C; D; E ]

let rows = [ One; Two; Three; Four; Five ]

let descriptions =
    [ "\"Can you hear me?\""
      "\"Can you mute your mic?\""
      "\"Your mic is on mute\""
      "\"Can you see me?\"" 
      "\"Can you see my screen?\""
      "\"Is {so-and-so} on the call?\""
      "\"I'm having technical difficulties\""
      "\"Did {so-and-so} leave the call?\""
      "\"Can everyone turn their cameras on?\""
      "\"I thought I was on mute\""
      "\"I'm going to put everyone on mute\""
      "\"Sorry, you go first\""
      "\"I'll be right back\""
      "\"Sorry, that was {family member}\""
      "\"Do you hear that?\""
      "\"Can you repeat that?\""
      "\"I'm having trouble hearing you\""
      "A dog barking"
      "A child walks in the room"
      "The video freezes"
      "A mic causes feedback"
      "A toilet flushes"
      "Background voices"
      "Someone's phone rings" ]

module List =
    let apply xs fs =
        let go state f =
            state @ (List.map f xs)         

        fs
        |> List.fold go []  

let createPosition column row =
    { Column = column
      Row = row }

let createTile position description status =
    { Position = position
      Description = description
      Status = status }

let randomize list =
    let rnd = Random()
    let rec swap n xs =
        match n with
        | n when n <= 0 -> xs
        | _ ->  
            
            let i = rnd.Next(List.length xs)
            let j = rnd.Next(List.length xs)

            let ys =
                xs
                |> List.mapi (fun index ele ->
                    if index = i then xs.[j]
                    elif index = j then xs.[i]
                    else ele)

            swap (n - 1) ys

    swap 100 list

let addFreeSpace list =
    let before, after = List.splitAt 12 list
    before @ ["Free Space"] @ after

let positions =
    columns 
    |> List.map createPosition
    |> List.apply rows

let generateTiles() =
    let combineIntoTile (position, description) =
        let status =
            if description = "Free Space"
            then FreeSpace
            else Unselected
        createTile position description status

    randomize descriptions
    |> addFreeSpace
    |> List.zip positions
    |> List.map combineIntoTile