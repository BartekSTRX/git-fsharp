module GitObjectsTests

open System.Text
open Xunit
open GitLib
open Utils

[<Fact>]
let ``Parse a commit without parents`` () =
    let commitStr = @"tree e6ed838ae8ebdc206381a819c87c8fdf9e58ea10
author Bartłomiej Kołodziejczyk <bartek.kol93@gmail.com> 1548609402 +0100
committer Bartłomiej Kołodziejczyk <bartek.kol93@gmail.com> 1548609402 +0100

first test commit
"
    let commitBytes = Encoding.UTF8.GetBytes commitStr
    let result = Commits.parseCommit commitBytes

    match result with
    | Ok commit -> true
    | Error reason -> failwith reason

    