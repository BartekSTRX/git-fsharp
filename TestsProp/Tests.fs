module Tests

open System.Linq
open Xunit
open FsCheck
open FsCheck.Xunit
open GitLib


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
let generateByte = 
    Gen.choose(0, 255) |> Gen.map byte
let generateBinaryHash =
    Gen.arrayOfLength 20 generateByte
type ByteArrayArb =
    static member Array() = Arb.fromGen(generateBinaryHash |> Gen.map BinarySha1)

[<Property( Arbitrary=[| typeof<ByteArrayArb> |] )>]
let ``Sha1 - deserialize and serialize`` (BinarySha1 hash) =
    let result = hash |> Hash.fromByteArray |> Hash.toByteArray 
    hash.SequenceEqual(result)


let generateBinaryDate =
    Gen.arrayOfLength 8 generateByte
let generateSha1 = 
    generateBinaryHash|> Gen.map Hash.fromByteArray 
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
    let createDefultEntry = Arb.Default.Derive<GitIndexEntry>().Generator //generate<GitIndexEntry>
    Gen.map5 createEntry
        createDefultEntry
        generateBinaryDate 
        generateBinaryDate 
        generateSha1
        generateNonemptyString


type GitIndexArb =
    static member Index() = Arb.fromGen(generateIndexEntry)

[<Property( Arbitrary=[| typeof<GitIndexArb> |] )>]
let ``GitIndex - serialize and parse`` (index: GitIndex) = 
    let result = index |> GitIndexes.serialize |> GitIndexes.parse

    match result with
    | Ok deserializedIndex -> index = deserializedIndex
    | Error _ -> false