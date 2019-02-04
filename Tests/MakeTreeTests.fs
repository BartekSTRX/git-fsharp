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
let ``Hash tree - 2 blobs, 1 subtree``() =
    let tree = {
        TreeEntries = [
            TreeEntry(Mode100644, Sha1 "acbe86c7c89586e0912a0a851bacf309c595c308", "aaa.txt")
            TreeEntry(Mode100644, Sha1 "edd8c0ab1a53fe79a16a94c1658bcd6477140eb9", "def.txt")
            TreeEntry(Mode040000, Sha1 "a8e9b07c6b091c1370838c441627d5721cb84d20", "gggg")
        ]
    }
    let hash = MakeTree.hashTree tree
    Assert.Equal(Sha1 "2fb593a7fe61740f89c408af7a84a1b85f0b1c95", hash)