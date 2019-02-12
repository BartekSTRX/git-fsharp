namespace GitLib

// symbolic reference like HEAD have following format
// ref: refs/heads/master
module SymbolicReferences =

    let formatSymRef (ref: string) = sprintf "ref: %s" ref

    let parseSymRef (refText: string) = refText.Substring(5)

