module GitIndexTests

open Xunit
open GitLib


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
            { sampleEntry with 
                RelativeFilePath = "aaa"
                Hash = Sha1 "10000000000000000000" }
            { sampleEntry with 
                RelativeFilePath = "bbb" 
                Hash = Sha1 "20000000000000000000" }
            { sampleEntry with 
                RelativeFilePath = @"ffff\cccc" 
                Hash = Sha1 "30000000000000000000" }
            { sampleEntry with 
                RelativeFilePath = @"ffff\dddd" 
                Hash = Sha1 "40000000000000000000" }
            { sampleEntry with 
                RelativeFilePath = @"ffff\ggggg\wwww"
                Hash = Sha1 "50000000000000000000" }
            { sampleEntry with 
                RelativeFilePath = @"ffff\hhhhh\zzzz"
                Hash = Sha1 "60000000000000000000" }
            { sampleEntry with 
                RelativeFilePath = @"hhhh\xxxx"
                Hash = Sha1 "70000000000000000000" }
            { sampleEntry with 
                RelativeFilePath = @"hhhh\yyyy" 
                Hash = Sha1 "80000000000000000000" }
        ]
    }
    let tree = MakeTree.getTree index

    let expectedTree = 
        IndexTreeModel [
            IndexBlobModel(Mode100644, Sha1 "10000000000000000000", "aaa")
            IndexBlobModel(Mode100644, Sha1 "20000000000000000000", "bbb")
            IndexSubTreeModel([
                IndexBlobModel(Mode100644, Sha1 "30000000000000000000", "cccc")
                IndexBlobModel(Mode100644, Sha1 "40000000000000000000", "dddd")
                IndexSubTreeModel([ 
                    IndexBlobModel(Mode100644, Sha1 "50000000000000000000", "wwww")
                ], "ggggg")
                IndexSubTreeModel([ 
                    IndexBlobModel(Mode100644, Sha1 "60000000000000000000", "zzzz") 
                ], "hhhhh")
            ], "ffff")
            IndexSubTreeModel ([
                IndexBlobModel(Mode100644, Sha1 "70000000000000000000", "xxxx")
                IndexBlobModel(Mode100644, Sha1 "80000000000000000000", "yyyy")
            ], "hhhh")
        ] 

    Assert.Equal(expectedTree, tree)
