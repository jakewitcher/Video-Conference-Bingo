[<AutoOpen>]
module ModuleExtensions

module List =
    let apply xs fs =
        let go state f =
            state @ (List.map f xs)         

        fs
        |> List.fold go []