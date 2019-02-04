namespace GitLib

type IndexEntryMode = 
    | Mode100644 // normal file
    | Mode100755 // executable
    | Mode120000 // symbolic link
    | Mode040000 // directory

module IndexEntryModes =
    let parse = function
        | "100644" -> Ok Mode100644
        | "100755" -> Ok Mode100755
        | "120000" -> Ok Mode120000
        | "040000" | "40000" -> Ok Mode040000
        | _ -> Error "unsupported mode"

    let toStr = function
        | Mode100644 -> "100644"
        | Mode100755 -> "100755"
        | Mode120000 -> "120000"
        | Mode040000 -> "040000"