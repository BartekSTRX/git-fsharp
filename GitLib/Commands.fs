module GitLib.Commands

open System
open System.IO
open System.Text
open GitLib.Models

let init (rootDir: string) : unit =
    let gitDir = Path.Combine(rootDir, ".git")

    let folders = [
        "hooks";
        "info";
        "objects/info";
        "objects/pack";
        "refs/heads";
        "refs/tags"
    ]
    for folder in folders do
        let path = Path.Combine(gitDir, folder)
        Directory.CreateDirectory(path) |> ignore

    let defaultConfig = @"[core]
repositoryformatversion = 0
filemode = false
bare = false
logallrefupdates = true
symlinks = false
ignorecase = true
"
    let configPath = Path.Combine(gitDir, "config")
    File.WriteAllText(configPath, defaultConfig, Encoding.UTF8)

    let defaultHead = "ref: refs/heads/master\n"
    let headPath = Path.Combine(gitDir, "HEAD")
    File.WriteAllText(headPath, defaultHead, Encoding.UTF8)


let hashObject (rootDir: string) (currentDir: string) (relativePath: string) (write: bool) : unit = 
    let objectPath = Path.Combine(currentDir, relativePath)

    let content = File.ReadAllBytes(objectPath)
    let wrappedObject = 
        { ObjectType = Blob; Object = content } 
        |> GitObjects.wrap

    let hash = wrappedObject |> Hash.sha1Bytes
    
    if write then 
        Storage.writeObjectContent rootDir hash wrappedObject

    let (Sha1 objectId) = hash
    objectId |> printf "%s"

// for now works only with blobs
let catFiles (rootDir: string) (option: string) (objectId: string) : unit = 
    objectId
    |> Hash.parse
    |> Result.bind (
        Storage.readObject rootDir 
        >> (fun x -> x.Content) 
        >> GitObjects.unwrap)
    |> Result.bind (fun result -> 
        match option with 
        | "-t" -> Ok (result.ObjectType |> ObjectTypes.toStr)
        | "-s" -> Ok (result.Size |> sprintf "%i")
        | "-p" -> Ok (result.Object |> Encoding.UTF8.GetString)
        | _ -> Error "incorrect cat-files option")
    |> (function 
        | Ok str -> str |> printf "%s"
        | Error err -> printf "%s" err)


type LsFileFormat = Default | ShowObjectNames

let private formatEntryShowNames (entry: GitIndexEntry) = 
    let {
        Mode = mode; 
        Hash = Sha1 id; 
        Flags = { Stage = stage }; 
        RelativeFilePath = path 
        } = entry
    sprintf "%i %s %i %s\n" mode id stage path

let private formatEntryDefault ({ RelativeFilePath = path }) =
    sprintf "%s\n" path

let private formatEntries entries format =
    let formatFun =
        match format with 
        | Default -> formatEntryDefault
        | ShowObjectNames -> formatEntryShowNames
    List.map formatFun entries 

// ls-files should list files from index and form working tree
let lsFiles (rootDir: string) (format: LsFileFormat) =
    Storage.readIndex rootDir
    |> Result.map(fun index -> formatEntries index.Entries format)
    |> Result.map (fun xs -> String.Join("", xs) )
    |> (function 
        | Ok str -> str |> printf "%s"
        | Error err -> printf "%s" err)
    
