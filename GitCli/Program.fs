open GitLib
open GitLib.Commands
open System

[<EntryPoint>]
let main argv =

    // TODO
    // write-tree
    // read-tree --prefix=bak
    // commit-tree
    // ls-tree
    // branch?
    // update-ref
    // symbolic-ref
    // log

    //let dir = Directory.GetCurrentDirectory()
    let dir = "C:\Users\LAPPEK4\Documents\heh1"

    Commands.catFiles dir "-p" "deeafcb1a078d8add9f9a6209d772e0f03b1de79"

    //Commands.lsFiles dir LsFileFormat.Default
    //Commands.updateIndexAdd dir dir (CacheInfo("100644", "8d5c3f86d71f9d9265b5b47a3b019cfed9cc46a7", "ggggf.txt"))

    Console.ReadLine() |> ignore

    let argsList = argv |> List.ofArray

    match argsList with 
    | ["init"] -> 
        Commands.init(dir)
    | ["hash-object"; relativePath] -> 
        Commands.hashObject dir dir relativePath false
    | ["hash-object"; "-w"; relativePath] -> 
        Commands.hashObject dir dir relativePath true
    | ["cat-file"; option; hash] ->
        Commands.catFiles dir option hash
    | ["ls-files"] ->
        Commands.lsFiles dir LsFileFormat.Default
    | ["ls-files"; "-s"] ->
        Commands.lsFiles dir LsFileFormat.ShowObjectNames
    | ["update-index"; "--add"; "--cacheinfo"; mode; hash; filePath] ->
        Commands.updateIndexAdd dir dir (CacheInfo(mode, hash, filePath))
    | ["write-tree"] ->
        ()
    | _ -> printf "incorrect args %A" argv

    0
