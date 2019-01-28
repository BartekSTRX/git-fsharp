open GitLib
open System.IO

[<EntryPoint>]
let main argv =

    // TODO
    // update-index --add --cache-info
    // write-tree
    // read-tree --prefix=bak
    // commit-tree

    let dir = Directory.GetCurrentDirectory()

    //let hash = "acbe86c7c89586e0912a0a851bacf309c595c308"
    //for option in ["-t"; "-s"; "-p"] do
    //    Commands.catFiles dir option hash
    //        |> printf "%s"

    match argv with 
    | [| "init" |] -> 
        Commands.init(dir)
    | [| "hash-object"; relativePath |] -> 
        Commands.hashObject dir dir relativePath false
    | [| "hash-object"; "-w"; relativePath |] -> 
        Commands.hashObject dir dir relativePath true
    | [| "cat-file"; option; hash |] ->
        Commands.catFiles dir option hash
    | _ -> printf "incorrect args %A" argv

    0
