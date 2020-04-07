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
    { BingoCard: BingoCard
      WinConditionMet: bool }

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
    { BingoCard = generateTiles()
      WinConditionMet = false }

let toggleSelected status =
    if status = Selected then Unselected
    elif status = Unselected then Selected
    else status

let isHorizontalWin tiles =
    tiles
    |> List.groupBy (fun tile -> tile.Position.Row)
    |> List.exists (fun (_,ts) -> List.length ts = 5)

let isVerticalWin tiles =
    tiles 
    |> List.groupBy (fun tile -> tile.Position.Column)
    |> List.exists (fun (_,ts) -> List.length ts = 5)

let isDiagonalWin tiles =
    let tilesSet =
        tiles
        |> List.map (fun tile -> tile.Position)
        |> Set.ofList

    let topLeftToBottomRight =
        [ { Column = A; Row = One }
          { Column = B; Row = Two }
          { Column = D; Row = Four }
          { Column = E; Row = Five } ]
        |> Set.ofList              

    let bottomLeftTopRight =
        [ { Column = A; Row = Five }
          { Column = B; Row = Four }
          { Column = D; Row = Two }
          { Column = E; Row = One } ]
        |> Set.ofList      

    Set.isSubset topLeftToBottomRight tilesSet || 
    Set.isSubset bottomLeftTopRight tilesSet

let checkWinCondition tiles =
    let isWinCondition tiles =
        isVerticalWin tiles ||
        isHorizontalWin tiles ||
        isDiagonalWin tiles
    
    tiles
    |> List.filter (fun tile -> tile.Status = Selected || tile.Status = FreeSpace)
    |> isWinCondition

let update (msg: Msg) (state: State): State =
    match msg with
    | ToggleSelection position -> 
        let nextCard =
            state.BingoCard
            |> List.map (fun tile ->
                if tile.Position = position
                then { tile with Status = toggleSelected tile.Status }
                else tile)

        { state with 
            BingoCard = nextCard
            WinConditionMet = checkWinCondition nextCard }

    | ResetBingoCard ->
        { state with 
            BingoCard = generateTiles()
            WinConditionMet = false }  

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

let title =
    Html.div [
        prop.style [ style.margin 15; style.padding 10 ]
        prop.classes [ Bulma.IsLarge; Bulma.IsFullwidth ]
        prop.children [
            Html.h1 [
                prop.classes [ Bulma.IsSize3; Bulma.HasTextWeightBold; Bulma.HasTextGreyDark; Bulma.IsFullwidth; Bulma.HasTextCentered ]
                prop.text "Video Conference Bingo"
            ]
        ]
    ]

let gameBoard (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.className Bulma.Container
        prop.children [ renderBingoCard state.BingoCard dispatch ]
    ]

let resetButton isWinCondition (dispatch: Msg -> unit) =
    Html.div [
        prop.className [ Bulma.Container; Bulma.IsFullwidth; Bulma.HasTextCentered ]
        prop.style [ style.paddingBottom 25; style.paddingTop 15 ]
        prop.children [
            Html.button [
                prop.className [ 
                    true, Bulma.Button
                    true, Bulma.HasTextDanger
                    true, Bulma.HasTextCentered
                    true, Bulma.IsLarge
                    isWinCondition, Bulma.HasBackgroundLight ]
                prop.text "Reset Bingo Card"
                prop.onClick (fun _ -> dispatch ResetBingoCard)
            ]
        ]
    ]

let icons = 
    [ "fas fa-hotdog"
      "fas fa-beer"
      "fas fa-ghost"
      "fas fa-kiwi-bird"
      "fas fa-fighter-jet"
      "fas fa-pizza-slice" ]

let snark =
    [ "Congratulations, your boss will be so proud."
      "I bet you feel productive now."
      "And to think, you're getting paid for this."
      "You should lie down after all that hard work."
      "Do your coworkers know how you spend your time?"
      "I hope this meeting wasn't important."
      "Keep this up and you'll definitely get that promotion!" ]

let pickRandom list =
    let rnd = Random()
    let index = rnd.Next(List.length list)

    list.[index]

let winView (dispatch: Msg -> unit) =
    Html.div [
        prop.style [ style.margin 55; style.marginBottom 75 ]
        prop.className [ Bulma.Hero; Bulma.IsCentered; Bulma.Box ]
        prop.children [
            Html.div [
                prop.className Bulma.HeroBody
                prop.children [
                    Html.div [
                        prop.className [ Bulma.HasTextDanger; Bulma.HasTextCentered; Bulma.IsFullwidth ]
                        prop.children [
                            Html.i [
                                prop.className [ (pickRandom icons) + " fa-5x fa-spin" ]
                            ] 
                        ]                        
                    ]
                                       

                    Html.h2 [
                        prop.style [ style.margin 10 ]
                        prop.classes [ Bulma.IsSize1; Bulma.HasTextWeightBold; Bulma.HasTextDanger; Bulma.IsFullwidth; Bulma.HasTextCentered ]
                        prop.text "You've won!"
                    ]

                    Html.p [
                        prop.style [ style.marginBottom 15 ]
                        prop.classes [ Bulma.IsSize4; Bulma.HasTextGreyDark; Bulma.IsFullwidth; Bulma.HasTextCentered ]
                        prop.text (pickRandom snark)
                    ]               
                ]
            ]
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.className [ state.WinConditionMet, Bulma.HasBackgroundGreyLight ]
        prop.children [
            title            
            if state.WinConditionMet
            then winView dispatch
            else gameBoard state dispatch
            resetButton state.WinConditionMet dispatch
        ]
    ]

    

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run