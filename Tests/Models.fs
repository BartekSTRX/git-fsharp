module ModelsTests

open System
open System.Text
open Xunit
open GitLib


//[<Fact>]
//let ``Hash blob object`` () =
//    let rootDir = Directory.GetCurrentDirectory()
//    let path = Path.Combine("TestData", "SampleTextFile.txt")

//    Commands.hashObject rootDir rootDir path false
//    Assert.Equal("26896bbf9c17fd5475973450ec83f7d8d84575bb", hash)


[<Fact>]
let ``Traverse index and convert it into tree structure `` () =
    let sampleEntry = {
        Ctime = [| 1uy; 2uy; 3uy; 4uy |]
        Mtime = [| 1uy; 2uy; 3uy; 4uy |]
        Device = 0u
        Inode = 0u
        Mode = Mode100644
        UserId = 0u
        GroupId = 0u
        FileSize = 123u
        Hash = Sha1 "12345678901234567890"
        Flags = { 
            AssumeValid = false
            Extended = false
            Stage = 0us
            NameLength = 43
        }
        RelativeFilePath = ""
    }

    let index = {
        Entries = [
            { sampleEntry with RelativeFilePath = "aaa" }
            { sampleEntry with RelativeFilePath = "bbb" }
            { sampleEntry with RelativeFilePath = @"ffff\cccc" }
            { sampleEntry with RelativeFilePath = @"ffff\dddd" }
            { sampleEntry with RelativeFilePath = @"ffff\ggggg\wwww" }
            { sampleEntry with RelativeFilePath = @"ffff\hhhhh\zzzz" }
            { sampleEntry with RelativeFilePath = @"hhhh\xxxx" }
            { sampleEntry with RelativeFilePath = @"hhhh\yyyy" }
        ]
    }
    let tree = GitIndexes.getTree index

    let expectedTree = 
        IndexSubTreeModel [
            IndexBlobModel
            IndexBlobModel
            IndexSubTreeModel [
                IndexBlobModel
                IndexBlobModel
                IndexSubTreeModel [ IndexBlobModel ]
                IndexSubTreeModel [ IndexBlobModel ]
            ]
            IndexSubTreeModel [
                IndexBlobModel
                IndexBlobModel
            ]
        ]

    Assert.Equal(expectedTree, tree)
