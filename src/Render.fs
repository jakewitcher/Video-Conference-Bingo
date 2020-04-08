module Render

open Feliz
open State
open System

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

module TitleView =
    let rendertitle =
        Html.h1 [
            prop.classes [ 
                Bulma.IsSize2
                Bulma.HasTextWeightBold
                Bulma.HasTextGreyDark
                Bulma.IsFullwidth
                Bulma.HasTextCentered 
            ]

            prop.text "Video Conference Bingo"
        ]

    let renderTitleView =
        Html.div [
            prop.style [ 
                style.margin 15
                style.padding 10 
            ]

            prop.classes [ 
                Bulma.IsLarge
                Bulma.IsFullwidth 
            ]

            prop.children [ 
                rendertitle 
            ]
        ]

module BingoCardView =
    let renderTileDescription tile =
        let isSelected = 
            tile.Status = Selected || 
            tile.Status = FreeSpace

        Html.p [
            prop.style [ 
                style.height 125
                style.display.flex
                style.alignItems.center
                style.justifyContent.center
            ]

            prop.className [ 
                true, Bulma.Box
                true, Bulma.HasTextCentered
                isSelected, Bulma.HasBackgroundDanger
                isSelected, Bulma.HasTextLight
                not isSelected, Bulma.HasBackgroundLight
                not isSelected, Bulma.HasTextGreyDark 
            ]

            prop.text tile.Description
        ]

    let renderTile tile dispatch =
        Html.div [
            prop.style [ 
                style.paddingLeft 10 
                style.paddingRight 10 
            ]

            prop.className [ 
                Bulma.Tile
                Bulma.IsChild 
            ]

            prop.children [ 
                renderTileDescription tile 
            ]

            prop.onClick (fun _ -> dispatch (ToggleSelection tile.Position))
        ]

    let renderRow row dispatch =
        Html.div [
            prop.style [ 
                style.marginLeft 15
                style.marginRight 15 
            ]

            prop.className [ 
                Bulma.Tile
                Bulma.IsParent 
            ]
            
            prop.children [
                for tile in row do 
                    renderTile tile dispatch
            ]
        ]

    let renderBingoCard card dispatch =
        let rows =
            List.chunkBySize 5 card
        
        Html.div [
            prop.style [ 
                style.margin 25
                style.marginLeft 35
                style.marginRight 35 
            ]

            prop.classes [ 
                Bulma.Tile
                Bulma.IsAncestor
                Bulma.IsVertical
                Bulma.Box 
            ]
            
            prop.children [
                for row in rows do
                    renderRow row dispatch
            ]
        ]

    let renderBingoCardView (state: State) (dispatch: Msg -> unit) =
        Html.div [
            prop.className Bulma.Container

            prop.children [ 
                renderBingoCard state.BingoCard dispatch 
            ]
        ]    

module ResetButtonView =
    let renderResetButton isWinCondition (dispatch: Msg -> unit) =
        Html.button [
            prop.className [ 
                true, Bulma.Button
                true, Bulma.HasTextDanger
                true, Bulma.HasTextCentered
                true, Bulma.IsLarge
                isWinCondition, Bulma.HasBackgroundLight 
            ]
            
            prop.onClick (fun _ -> dispatch ResetBingoCard)
            prop.text "Reset Bingo Card"
        ]

    let renderResetButtonView isWinCondition (dispatch: Msg -> unit) =
        Html.div [
            prop.style [ 
                style.paddingBottom 25
                style.paddingTop 15
            ]

            prop.className [ 
                Bulma.Container
                Bulma.IsFullwidth
                Bulma.HasTextCentered
            ]
            
            prop.children [
                renderResetButton isWinCondition dispatch
            ]
        ]

module WinConditionView =
    let pickRandom list =
        let rnd = Random()
        let index = rnd.Next(List.length list)

        list.[index]

    let renderWinIcon =
        Html.i [
            prop.className (sprintf "%s fa-5x fa-spin" (pickRandom icons))
        ]

    let renderWinIconView =
        Html.div [
            prop.className [ 
                Bulma.HasTextDanger
                Bulma.HasTextCentered
                Bulma.IsFullwidth
            ]

            prop.children [
                 renderWinIcon
            ]                        
        ]

    let renderAnnouncementView =
        Html.h2 [
            prop.style [ 
                style.margin 10 
            ]

            prop.classes [ 
                Bulma.IsSize1
                Bulma.HasTextWeightBold
                Bulma.HasTextDanger
                Bulma.IsFullwidth
                Bulma.HasTextCentered 
            ]

            prop.text "You've won!"
        ]

    let renderSnarkCommentView =
        Html.p [
            prop.style [ style.marginBottom 15 ]
            prop.classes [ Bulma.IsSize4; Bulma.HasTextGreyDark; Bulma.IsFullwidth; Bulma.HasTextCentered ]
            prop.text (pickRandom snark)
        ]

    let renderWinBodyView =
        Html.div [
            prop.className Bulma.HeroBody
            prop.children [
                renderWinIconView
                renderAnnouncementView                               
                renderSnarkCommentView              
            ]
        ]

    let renderWinConditionView (dispatch: Msg -> unit) =
        Html.div [
            prop.style [ 
                style.margin 55
                style.marginBottom 75 
            ]

            prop.className [ 
                Bulma.Hero
                Bulma.IsCentered
                Bulma.Box 
            ]

            prop.children [
                renderWinBodyView
            ]
        ]

let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.style [
            style.margin 25
            style.padding 15
            style.paddingBottom 25          
        ]

        prop.className [ 
            true, Bulma.IsFullheight
            state.WinConditionMet, Bulma.HasBackgroundGreyLight
            not state.WinConditionMet, Bulma.HasBackgroundWhiteBis
        ]

        prop.children [
            TitleView.renderTitleView            
            if state.WinConditionMet
            then WinConditionView.renderWinConditionView dispatch
            else BingoCardView.renderBingoCardView state dispatch
            ResetButtonView.renderResetButtonView state.WinConditionMet dispatch
        ]
    ]
