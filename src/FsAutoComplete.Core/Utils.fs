[<AutoOpen>]
module FsAutoComplete.Utils

open System.IO

type Result<'a> =
  | Success of 'a
  | Failure of string

type Pos = 
    { Line: int
      Col: int }

type Serializer = obj -> string

type ProjectFilePath = string
type SourceFilePath = string
type LineStr = string

let isAScript fileName =
    let ext = Path.GetExtension fileName
    [".fsx";".fsscript";".sketchfs"] |> List.exists ((=) ext)

let runningOnMono = 
  try System.Type.GetType("Mono.Runtime") <> null
  with _ -> false

let normalizePath (file : string) = 
  if file.EndsWith ".fs" then 
      let p = Path.GetFullPath file
      (p.Chars 0).ToString().ToLower() + p.Substring(1)
  else file
  
let inline combinePaths path1 (path2 : string) = Path.Combine(path1, path2.TrimStart [| '\\'; '/' |])

let inline (</>) path1 path2 = combinePaths path1 path2

module Option =
  let getOrElse defaultValue option =
    match option with
    | None -> defaultValue
    | Some x -> x

  let getOrElseFun defaultValue option =
    match option with
    | None -> defaultValue()
    | Some x -> x

module Async =
    /// Transforms an Async value using the specified function.
    [<CompiledName("Map")>]
    let map (mapping : 'a -> 'b) (value : Async<'a>) : Async<'b> =
        async {
            // Get the input value.
            let! x = value
            // Apply the mapping function and return the result.
            return mapping x
        }

    // Transforms an Async value using the specified Async function.
    [<CompiledName("Bind")>]
    let bind (binding : 'a -> Async<'b>) (value : Async<'a>) : Async<'b> =
        async {
            // Get the input value.
            let! x = value
            // Apply the binding function and return the result.
            return! binding x
        }

    [<RequireQualifiedAccess>]    
    module Array =
        /// Async implementation of Array.map.
        let map (mapping : 'T -> Async<'U>) (array : 'T[]) : Async<'U[]> =
            let len = Array.length array
            let result = Array.zeroCreate len

            async { // Apply the mapping function to each array element.
                for i in 0 .. len - 1 do
                    let! mappedValue = mapping array.[i]
                    result.[i] <- mappedValue

                // Return the completed results.
                return result
            }

        /// Async implementation of Array.mapi.
        let mapi (mapping : int -> 'T -> Async<'U>) (array : 'T[]) : Async<'U[]> =
            let len = Array.length array
            let result = Array.zeroCreate len

            async {
                // Apply the mapping function to each array element.
                for i in 0 .. len - 1 do
                    let! mappedValue = mapping i array.[i]
                    result.[i] <- mappedValue

                // Return the completed results.
                return result
            }

        /// Async implementation of Array.exists.
        let exists (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<bool> =
            let len = Array.length array
            let rec loop i =
                async {
                    if i >= len then
                        return false
                    else
                        let! found = predicate array.[i]
                        if found then
                            return true
                        else
                            return! loop (i + 1)
                }
            loop 0

module List =
    let inline singleton x = [x]

type System.Collections.Concurrent.ConcurrentDictionary<'key, 'value> with
    member x.TryFind key =
        match x.TryGetValue key with
        | true, value -> Some value
        | _ -> None

    member x.ToSeq() =
        x |> Seq.map (fun (KeyValue(k, v)) -> k, v)

let inline debug msg = Printf.kprintf System.Diagnostics.Debug.WriteLine msg
let inline fail msg = Printf.kprintf System.Diagnostics.Debug.Fail msg