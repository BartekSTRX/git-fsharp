open System

[<EntryPoint>]
let main argv =
    
    match argv with 
    | [| "init" |] -> GitLib.Commands.init()
    | _ -> printf "incorrect args %A" argv

    0
