module CommandInit

open Xunit
open GitLib
open System.IO

[<Fact>]
let ``Create empty repository - files are saved on disk`` () =
    let repoDirName = "testRepo"
    let repoFullPath = Path.Combine(Config.rootDir, repoDirName)

    Commands.init repoFullPath

    Assert.True(Directory.Exists(repoFullPath))

    Directory.Delete(repoFullPath, recursive=true)