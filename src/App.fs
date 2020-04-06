module App

open Elmish
open Elmish.React
open Feliz
open System

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
    { BingoCard: BingoCard }

type Msg =
    | ToggleSelection of Position
    | ResetBingoCard

let createPosition column row =
    { Column = column
      Row = row }

let createTile position description status =
    { Position = position
      Description = description
      Status = status }

let randomize list =
    let rnd = Random()
    let rec go n xs =
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

            go (n - 1) ys

    go 100 list

let addFreeDescription list =
    if List.length list <> 24 then list
    else 
        let before, after = List.splitAt 12 list
        before @ ["Free Space"] @ after

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
      "\"Sorry, that was my {family member}\""
      "\"Do you hear that?\""
      "\"Can you repeat that?\""
      "\"I'm having trouble hearing you\""
      "A dog barks in the background"
      "A child walks in the room"
      "Someone's video freezes"
      "Someone's mic causes feedback"
      "A toilet flushes"
      "You hear someone talking in the background"
      "Someone's phone rings"    
      ]

let positions =
    columns 
    |> List.map createPosition
    |> List.apply rows

let generateTiles() =
    randomize descriptions
    |> addFreeDescription
    |> List.zip positions
    |> List.map (fun (position, description) ->
        let status =
            if description = "Free Space"
            then FreeSpace
            else Unselected
        createTile position description status)

let init() =
    { BingoCard = generateTiles() }

let toggleSelected status =
    if status = Selected then Unselected
    elif status = Unselected then Selected
    else status

let update (msg: Msg) (state: State): State =
    match msg with
    | ToggleSelection position -> 
        let nextCard =
            state.BingoCard
            |> List.map (fun tile ->
                if tile.Position = position
                then { tile with Status = toggleSelected tile.Status }
                else tile)

        { state with BingoCard = nextCard }

    | ResetBingoCard ->
        { state with BingoCard = generateTiles() }  

let renderTile tile dispatch =
    Html.div [
        prop.style [ style.paddingLeft 10; style.paddingRight 10 ]
        prop.className [ 
            true, Bulma.Tile
            true, Bulma.IsChild
            // true, Bulma.Box
             
        ]
        prop.onClick (fun _ -> dispatch (ToggleSelection tile.Position))
        prop.children [ 
            Html.p [
                prop.style [ style.height 100; style.padding 20 ]
                prop.className [ 
                    true, Bulma.Box
                    true, Bulma.HasTextCentered
                    tile.Status = FreeSpace, Bulma.HasBackgroundDanger
                    tile.Status = Selected, Bulma.HasBackgroundDanger 
                    tile.Status = Unselected, Bulma.HasBackgroundLight
                    tile.Status = FreeSpace, Bulma.HasTextLight
                    tile.Status = Selected, Bulma.HasTextLight
                    tile.Status = Unselected, Bulma.HasTextGreyDark ]
                prop.children [
                    Html.text tile.Description
                ]
            ]
        ]
    ]

let renderRow row dispatch =
    Html.div [
        prop.style [ style.marginLeft 15; style.marginRight 15 ]
        prop.classes [ Bulma.Tile; Bulma.IsParent ]
        prop.children [
            for tile in row do 
                renderTile tile dispatch
        ]
    ]

let renderBingoCard card dispatch =
    let rows =
        card
        |> List.chunkBySize 5
    
    Html.div [
        prop.classes [ Bulma.Tile; Bulma.IsAncestor; Bulma.IsVertical; Bulma.Box ]
        prop.style [ style.margin 25; style.marginLeft 35; style.marginRight 35 ]
        prop.children [
            for row in rows do
                renderRow row dispatch
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.children [
            Html.div [
                prop.style [ style.margin 15 ]
                prop.classes [ Bulma.Container; Bulma.IsLarge; Bulma.HasTextCentered ]
                prop.children [
                    Html.h1 [
                        prop.classes [ Bulma.IsSize3; Bulma.HasTextWeightBold; Bulma.HasTextGreyDark ]
                        prop.text "Video Conference Bingo"
                    ]
                ]
            ]

            Html.div [
                prop.className Bulma.Container
                prop.children [ renderBingoCard state.BingoCard dispatch ]
            ]

            Html.div [
                prop.className [ Bulma.Container; Bulma.IsFullwidth; Bulma.HasTextCentered ]
                prop.style [ style.paddingBottom 25 ]
                prop.children [
                    Html.button [
                        prop.className [ Bulma.Button; Bulma.HasTextDanger; Bulma.HasTextCentered; Bulma.IsLarge ]
                        prop.text "Reset Bingo Card"
                        prop.onClick (fun _ -> dispatch ResetBingoCard)
                    ]
                ]
            ]
        ]
    ]

    

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run