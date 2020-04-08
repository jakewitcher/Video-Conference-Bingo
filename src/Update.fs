module Update

open Initialize
open State

module WinCondition =
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
            List.zip [ A; B; D; E ] [ One; Two; Four; Five ]
            |> List.map (fun (column, row) -> createPosition column row)
            |> Set.ofList       

        let bottomLeftToTopRight =
            List.zip [ A; B; D; E ] [ Five; Four; Two; One ]
            |> List.map (fun (column, row) -> createPosition column row)
            |> Set.ofList      

        Set.isSubset topLeftToBottomRight tilesSet || 
        Set.isSubset bottomLeftToTopRight tilesSet

    let check tiles =
        let isSelected tile =
            tile.Status = Selected ||
            tile.Status = FreeSpace

        let isWinCondition tiles =
            isVerticalWin tiles ||
            isHorizontalWin tiles ||
            isDiagonalWin tiles
        
        tiles
        |> List.filter isSelected
        |> isWinCondition

let toggle status =
    if status = Selected then Unselected
    elif status = Unselected then Selected
    else status

let toggleSelected position tile =
    if tile.Position = position
    then { tile with Status = toggle tile.Status }
    else tile

let update (msg: Msg) (state: State): State =
    match msg with
    | ToggleSelection position -> 
        let nextCard =
            state.BingoCard
            |> List.map (toggleSelected position)

        { state with 
            BingoCard = nextCard
            WinConditionMet = WinCondition.check nextCard }

    | ResetBingoCard ->
        { state with 
            BingoCard = generateTiles()
            WinConditionMet = false }  