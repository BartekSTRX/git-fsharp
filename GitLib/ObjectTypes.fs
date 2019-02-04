namespace GitLib

type ObjectType = Blob | Tree | Commit

module ObjectTypes =
    let toStr = function
        | Blob -> "blob"
        | Tree -> "tree"
        | Commit -> "commit"

    let fromStr = function
        | "blob" -> Ok Blob
        | "tree" -> Ok Tree
        | "commit" -> Ok Commit
        | _ -> Error "incorrect git object type"
