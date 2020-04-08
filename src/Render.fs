module Render

open Feliz
open State
open System

let div (classes: string list) (children: ReactElement list) =
  Html.div [
    prop.className classes
    prop.children children
  ]

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
      "Keep this up and you'll definitely get that promotion." ]

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
