module GitLib.Commands

open System.IO

let init () =
    let dir = Directory.GetCurrentDirectory()
    let gitDir = Path.Combine(dir, ".git")

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


