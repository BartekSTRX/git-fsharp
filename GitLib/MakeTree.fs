namespace GitLib

open System


type IndexTreeModelEntry = 
| IndexBlobModel of UnixFileMode * Sha1 * fileName:string
| IndexSubTreeModel of IndexTreeModelEntry list * subDirName:string

type IndexTreeModel = IndexTreeModel of IndexTreeModelEntry list


module MakeTree =
    let getTree { Entries = entries } : IndexTreeModel = 
        let splitPath (path: string) = 
            path.Split([| '\\'; '/' |], StringSplitOptions.RemoveEmptyEntries)
            |> List.ofArray

        let rec traverseIndex (entriesWithPaths: (string list * GitIndexEntry) list): IndexTreeModelEntry list =
            let subTrees, blobs = entriesWithPaths |> List.partition (fun (ps, _) -> ps.Length > 1)

            let thisSubTreeBlobs =
                blobs 
                |> List.map (fun ([filename], indexEntry) -> 
                    IndexBlobModel(indexEntry.Mode, indexEntry.Hash, filename))
            let thisSubTreeSubTrees = 
                subTrees 
                |> List.groupBy (fun (root :: _path, _entry) -> root)
                |> List.map (fun (root, entries) -> 
                    let subEntries = 
                        entries 
                        |> List.map (fun (_root :: path, entry) -> (path, entry)) 
                        |> traverseIndex
                    IndexSubTreeModel(subEntries, root))
            List.append thisSubTreeBlobs thisSubTreeSubTrees
        
        entries 
        |> List.map (fun e -> (splitPath e.RelativeFilePath, e))
        |> traverseIndex
        |> IndexTreeModel


    let hashTree = 
        Trees.serializeTree 
        >> GitObjects.wrap
        >> Hash.sha1Bytes

    let createTreeObjects (IndexTreeModel(modelEntries)) : Tree list =
        let rec traverseModel (model: IndexTreeModelEntry) : TreeEntry * Tree list =
            match model with
            | IndexBlobModel(mode, hash, fileName) -> 
                TreeEntry(mode, hash, fileName), []
            | IndexSubTreeModel(entryList, subDirName) ->
                let subEntries, createdTrees = entryList |> List.map traverseModel |> List.unzip
                let newTree = { TreeEntries = subEntries }
                let treeHash = newTree |> hashTree

                let newEntry = TreeEntry(Mode040000, treeHash, subDirName)
                let allTrees = List.concat (seq { yield [newTree]; yield! createdTrees })
                newEntry, allTrees
        let _rootEntry, trees = 
            IndexSubTreeModel(modelEntries, "")
            |> traverseModel
        trees
      