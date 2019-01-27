open GitLib
open GitLib.Models
open System.Text

[<EntryPoint>]
let main argv =
    //let dir = Directory.GetCurrentDirectory()

    let dir = @"C:\Users\LAPPEK4\Documents\heh2"
    //let filename = "aaa.txt"

    let hash = "acbe86c7c89586e0912a0a851bacf309c595c308"
    for option in ["-t"; "-s"; "-p"] do
        Commands.catFiles dir option hash
            |> printf "%s"

    match argv with 
    | [| "init" |] -> 
        Commands.init(dir)
    | [| "hash-object"; relativePath |] -> 
        Commands.hashObject dir relativePath
        |> printf "%s"
    | [| "cat-file"; option; hash |] ->
        Commands.catFiles dir option hash
        |> printf "%s"
    | _ -> printf "incorrect args %A" argv

    0
