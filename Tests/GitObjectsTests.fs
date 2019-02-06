module GitObjectsTests

open System.Text
open Xunit
open GitLib

[<Fact>]
let ``Parse a commit without parents`` () =
    let commitStr = @"tree e6ed838ae8ebdc206381a819c87c8fdf9e58ea10
author Bartłomiej Kołodziejczyk <bartek.kol93@gmail.com> 1548609402 +0100
committer Bartłomiej Kołodziejczyk <bartek.kol93@gmail.com> 1548609402 +0100

first test commit
"
    let commitBytes = Encoding.UTF8.GetBytes commitStr
    let result = Commits.parseCommit { ObjectType = Commit; Object = commitBytes }

    match result with
    | Ok { Tree = Sha1 treeId; Author = { Name = authorName }; Parents = [] } -> 
        Assert.Equal("e6ed838ae8ebdc206381a819c87c8fdf9e58ea10", treeId)
        Assert.Equal("Bartłomiej Kołodziejczyk", authorName)
    | Ok _ -> failwith "incorrect commit parents"
    | Error reason -> failwith reason


[<Fact>]
let ``Parse a commit with one parent`` () =
    let commitStr = @"tree 2fb593a7fe61740f89c408af7a84a1b85f0b1c95
parent 504b00c3b4fe52270904541cb6dd84cbd2a67e02
author Bartłomiej Kołodziejczyk <bartek.kol93@gmail.com> 1549300263 +0100
committer Bartłomiej Kołodziejczyk <bartek.kol93@gmail.com> 1549300263 +0100

second commit
"
    let commitBytes = Encoding.UTF8.GetBytes commitStr
    let result = Commits.parseCommit { ObjectType = Commit; Object = commitBytes }

    match result with
    | Ok { Tree = Sha1 treeId; Author = { Name = authorName }; Parents = [ Sha1 parent ] } -> 
        Assert.Equal("2fb593a7fe61740f89c408af7a84a1b85f0b1c95", treeId)
        Assert.Equal("Bartłomiej Kołodziejczyk", authorName)
        Assert.Equal("504b00c3b4fe52270904541cb6dd84cbd2a67e02", parent)
    | Ok _ -> failwith "incorrect commit parents"
    | Error reason -> failwith reason


[<Fact>]
let ``Commit - serialize and parse`` () = 
    let commit = {
        Tree = Sha1 "ff16a9c9ed2a7b2364bfe91397cb699f7e1584fa";
        Parents = [Sha1 "54de7fa00021984548f57a080e95aed0950bd7f9"];
        Author = {
            Name = "ab";
            Email = "b@da";
            Date = Some { DateSeconds = 2L; DateTimeZone = "+1000" }
        };
        Commiter = {
            Name = "a";
            Email = "da@bd";
            Date = None
        };
        Message = "db";
    }

    let serialized = commit |> Commits.serializeCommit
    let result = serialized |> Commits.parseCommit
    match result with 
    | Ok deserializedCommit -> commit = deserializedCommit
    | Error reason -> failwith reason