open GitLib
open GitLib.Commands

[<EntryPoint>]
let main argv =

    // TODO
    // update-index --add --cache-info
    // write-tree
    // read-tree --prefix=bak
    // commit-tree

    //let dir = Directory.GetCurrentDirectory()
    let dir = "C:\Users\LAPPEK4\Documents\heh1"

    Commands.lsFiles dir LsFileFormat.Default

    Commands.updateIndexAdd dir dir (CacheInfo("100644", "8d5c3f86d71f9d9265b5b47a3b019cfed9cc46a7", "ggggf.txt"))

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
