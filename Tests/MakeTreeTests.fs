module MakeTreeTests

open GitLib
open Xunit

[<Fact>]
let ``Hash tree - one blob`` () =
    let tree = {
        TreeEntries = [
            TreeEntry(Mode100644, Sha1 "8b69265c0d62250927fc2e06650652de5e6a925b", "ttyu.txt")
        ]
    }
    let hash = MakeTree.hashTree tree
    Assert.Equal(Sha1 "a8e9b07c6b091c1370838c441627d5721cb84d20", hash)

[<Fact>]
let ``Hash tree - two blobs`` () =
    let tree = {
        TreeEntries = [
            TreeEntry(Mode100644, Sha1 "13874f578d45e7e56874f289262baecabd55673d", "rererere.txt")
            TreeEntry(Mode100644, Sha1 "8b69265c0d62250927fc2e06650652de5e6a925b", "ttyu.txt")
        ]
    }
    let hash = MakeTree.hashTree tree
    Assert.Equal(Sha1 "d9e623af0d668996369dc70a6c764f2ac7fcf3f0", hash)

[<Fact>]
let ``Hash tree - 2 blobs, 1 subtree`` () =
    let tree = {
        TreeEntries = [
            TreeEntry(Mode100644, Sha1 "acbe86c7c89586e0912a0a851bacf309c595c308", "aaa.txt")
            TreeEntry(Mode100644, Sha1 "edd8c0ab1a53fe79a16a94c1658bcd6477140eb9", "def.txt")
            TreeEntry(Mode040000, Sha1 "d9e623af0d668996369dc70a6c764f2ac7fcf3f0", "gggg")
        ]
    }
    let hash = MakeTree.hashTree tree
    Assert.Equal(Sha1 "7b6ad422b933f250c8ae38f5e213385aefc55226", hash)


[<Fact>]
let ``Create tree objects for directory structure`` () =
    let treeModel = IndexTreeModel [
        IndexBlobModel(Mode100644, Sha1 "acbe86c7c89586e0912a0a851bacf309c595c308", "aaa.txt")
        IndexBlobModel(Mode100644, Sha1 "edd8c0ab1a53fe79a16a94c1658bcd6477140eb9", "def.txt")
        IndexSubTreeModel([
            IndexBlobModel(Mode100644, Sha1 "13874f578d45e7e56874f289262baecabd55673d", "rererere.txt")
            IndexBlobModel(Mode100644, Sha1 "8b69265c0d62250927fc2e06650652de5e6a925b", "ttyu.txt")
        ], "gggg")
    ]

    let result = MakeTree.createTreeObjects treeModel

    let expected = [
        {
            TreeEntries = [
                TreeEntry(Mode100644, Sha1 "acbe86c7c89586e0912a0a851bacf309c595c308", "aaa.txt")
                TreeEntry(Mode100644, Sha1 "edd8c0ab1a53fe79a16a94c1658bcd6477140eb9", "def.txt")
                TreeEntry(Mode040000, Sha1 "d9e623af0d668996369dc70a6c764f2ac7fcf3f0", "gggg")
            ]
        }
        { 
            TreeEntries = [ 
                TreeEntry(Mode100644, Sha1 "13874f578d45e7e56874f289262baecabd55673d", "rererere.txt")
                TreeEntry(Mode100644, Sha1 "8b69265c0d62250927fc2e06650652de5e6a925b", "ttyu.txt")
            ]
        }
    ]

    Assert.Equal<Tree list>(expected, result)