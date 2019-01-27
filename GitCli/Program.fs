open GitLib

[<EntryPoint>]
let main argv =

    // TODO
    // update-index --add --cache-info
    // write-tree
    // read-tree --prefix=bak
    // commit-tree

    //let dir = Directory.GetCurrentDirectory()

    let dir = @"C:\Users\LAPPEK4\Documents\heh2"

    //let hash = "acbe86c7c89586e0912a0a851bacf309c595c308"
    //for option in ["-t"; "-s"; "-p"] do
    //    Commands.catFiles dir option hash
    //        |> printf "%s"

    match argv with 
    | [| "init" |] -> 
        Commands.init(dir)
    | [| "hash-object"; relativePath |] -> 
        Commands.hashObject dir dir relativePath false
        |> printf "%s"
    | [| "hash-object"; "-w"; relativePath |] -> 
        let hash = Commands.hashObject dir dir relativePath true
        printf "%s" hash
    | [| "cat-file"; option; hash |] ->
        Commands.catFiles dir option hash
        |> printf "%s"
    | _ -> printf "incorrect args %A" argv

    0
