module Tests

open System.Linq
open FsCheck
open FsCheck.Xunit
open GitLib


let generateByte = 
    Gen.choose(0, 255) |> Gen.map byte

let generateBinaryHash =
    Gen.arrayOfLength 20 generateByte

let generateBinaryDate =
    Gen.arrayOfLength 8 generateByte

let generateSha1 = 
    generateBinaryHash |> Gen.map Hash.fromByteArray 

let generateNonemptyString = 
    [ 'a'; 'b'; 'd' ]
    |> Gen.elements
    |> Gen.nonEmptyListOf
    |> Gen.map Array.ofList
    |> Gen.map (fun x -> new string(x))

let generateIndexEntry : Gen<GitIndexEntry> =
    let createEntry entry ctime mtime hash path = 
        { entry with 
            Ctime = ctime
            Mtime = mtime
            Hash = hash
            Flags = { entry.Flags with NameLength = path |> String.length }
            RelativeFilePath = path }
    let createDefultEntry = Arb.Default.Derive<GitIndexEntry>().Generator
    Gen.map5 createEntry
        createDefultEntry
        generateBinaryDate 
        generateBinaryDate 
        generateSha1
        generateNonemptyString

let generateTreeEntry : Gen<TreeEntry> = 
    let modeGenerator = Arb.Default.Derive<IndexEntryMode>().Generator
    let createEntry m s p = TreeEntry(m, s, p)
    Gen.map3 createEntry modeGenerator generateSha1 generateNonemptyString

type TreeGenerator = 
    //static member TreeEntry() = Arb.fromGen(generateTreeEntry)
    static member Tree() = 
        generateTreeEntry 
        |> Gen.nonEmptyListOf 
        |> Gen.map (fun es -> { TreeEntries = es })
        |> Arb.fromGen


[<Property>]
let ``ObjectType - toStr and parse`` (object: ObjectType) = 
    let result = object |> ObjectTypes.toStr  |> ObjectTypes.fromStr
    match result with 
    | Ok t -> t = object
    | Error _ -> false


[<Property>]
let ``GitObject - wrap and unwrap`` (object: GitObject) = 
    let result = object |> GitObjects.wrap  |> GitObjects.unwrap
    match result with 
    | Ok { ObjectType = t; Object = content } -> 
        (t = object.ObjectType) && content.SequenceEqual(object.Object)
    | Error _ -> false


type BinarySha1 = BinarySha1 of byte[]
type ByteArrayArb =
    static member Array() = Arb.fromGen(generateBinaryHash |> Gen.map BinarySha1)

[<Property( Arbitrary=[| typeof<ByteArrayArb> |] )>]
let ``Sha1 - deserialize and serialize`` (BinarySha1 hash) =
    let result = hash |> Hash.fromByteArray |> Hash.toByteArray 
    hash.SequenceEqual(result)


type GitIndexArb =
    static member Index() = Arb.fromGen(generateIndexEntry)

[<Property( Arbitrary=[| typeof<GitIndexArb> |] )>]
let ``GitIndex - serialize and parse`` (index: GitIndex) = 
    let result = index |> GitIndexes.serialize |> GitIndexes.parse
    match result with
    | Ok deserializedIndex -> index = deserializedIndex
    | Error _ -> false


[<Property(Arbitrary=[| typeof<TreeGenerator> |])>]
let ``Tree - serialize to bytes and parse`` (tree: Tree) =
    let serialized = tree |> Trees.serializeTree
    let result = serialized |> Trees.parseTree
    match result with
    | Ok deserializedTree -> tree = deserializedTree
    | Error _ -> false
