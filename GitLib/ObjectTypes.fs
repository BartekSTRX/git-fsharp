namespace GitLib

type ObjectType = Blob | Tree | Commit | Tag

module ObjectTypes =
    let toStr = function
        | Blob -> "blob"
        | Tree -> "tree"
        | Commit -> "commit"
        | Tag -> "tag"

    let fromStr = function
        | "blob" -> Ok Blob
        | "tree" -> Ok Tree
        | "commit" -> Ok Commit
        | "tag" -> Ok Tag
        | _ -> Error "incorrect git object type"
