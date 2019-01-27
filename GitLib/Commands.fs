module GitLib.Commands

open System.IO
open GitLib.Models
open System.Text
open System

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


let hashObject (currentDir: string) (relativePath: string): unit = 
    let objectPath = Path.Combine(currentDir, relativePath)
    let content = File.ReadAllBytes(objectPath)

    let hash = content |> Hash.sha1Bytes
    let chars =
        hash 
        |> Array.collect (fun x -> 
            [| (x &&& byte(0b11110000)) >>> 4; x &&& byte(0b00001111) |])
        |> Array.map (sprintf "%X")
        

    String.Join("", chars)
//    |> Encoding.Unicode.GetString
    |> printf "%s"