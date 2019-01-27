module GitLib.Commands

open System
open System.IO
open GitLib.Models
open System.Text

let init (rootDir: string) =
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
    File.WriteAllText(configPath, defaultConfig)

    let defaultHead = "ref: refs/heads/master\n"
    let headPath = Path.Combine(gitDir, "HEAD")
    File.WriteAllText(headPath, defaultHead)


let hashObject (rootDir: string) (currentDir: string) (relativePath: string) (write: bool) : Sha1 = 
    let objectPath = Path.Combine(currentDir, relativePath)

    let content = File.ReadAllBytes(objectPath)
    let gitObject = { ObjectType = Blob; Object = content }
    let wrappedObject = gitObject |> GitObjects.wrap

    let hash = wrappedObject |> Hash.sha1Bytes
    
    if write then 
        Storage.writeObjectContent rootDir hash wrappedObject

    hash

let catFiles (rootDir: string) (option: string) (objectId: string) : string = 
    Hash.parse objectId
    |> Result.bind (
        fun hash -> 
            let object = Storage.readObject rootDir hash
            let unwrapped = GitObjects.unwrap object.Content
            unwrapped)
    |> Result.map (fun result -> 
        match option with 
        | "-t" -> sprintf "%s" (result.ObjectType |> ObjectTypes.toStr)
        | "-s" -> sprintf "%i" result.Size
        | "-p" -> sprintf "%s" (result.Object |> Encoding.UTF8.GetString)
        | _ -> failwith "incorrect cat-files option")
    |> (fun result ->
            match result with 
            | Ok str -> str
            | Error reason -> failwith reason)