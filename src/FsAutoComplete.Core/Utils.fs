[<AutoOpen>]
module FsAutoComplete.Utils

open System
open System.IO
open ExtCore

type Result<'a> =
  | Success of 'a
  | Failure of string

module Result =
    let map f = function
        | Success x -> Success (f x)
        | Failure e -> Failure e

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

  let inline tryCast<'T> (o: obj): 'T option =
    match o with
    | null -> None
    | :? 'T as a -> Some a
    | _ -> None

  let inline cast o =
    (Option.bind tryCast) o

  /// Convert string into Option string where null and String.Empty result in None
  let inline ofString (s:string) =
    if String.isNullOrEmpty s then None
    else Some s

  /// Some(Some x) -> Some x | None -> None
  let inline flatten x =
    match x with
    | Some x -> x
    | None -> None

  /// Gets the option if Some x, otherwise try to get another value
  let inline orTry f =
    function
    | Some x -> Some x
    | None -> f()


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

module List =
    let inline singleton x = [x]
        
    ///Returns the greatest of all elements in the list that is less than the threshold
    let maxUnderThreshold nmax =
        List.maxBy(fun n -> if n > nmax then 0 else n)

type System.Collections.Concurrent.ConcurrentDictionary<'key, 'value> with
    member x.TryFind key =
        match x.TryGetValue key with
        | true, value -> Some value
        | _ -> None

    member x.ToSeq() =
        x |> Seq.map (fun (KeyValue(k, v)) -> k, v)

let inline debug msg = Printf.kprintf System.Diagnostics.Debug.WriteLine msg
let inline fail msg = Printf.kprintf System.Diagnostics.Debug.Fail msg

module Seq =
    let tryHead items =
        if Seq.isEmpty items then None else Some (Seq.head items)
        
module String =
  open System.Text

  /// Split a line so it fits to a line width
  let splitLine (sb : StringBuilder) (line : string) lineWidth =
      let emit (s : string) = sb.Append(s) |> ignore

      let indent =
          line
          |> Seq.takeWhile (fun c -> c = ' ')
          |> Seq.length

      let words = line.Split(' ')
      let mutable i = 0
      let mutable first = true
      for word in words do
          if first || i + word.Length < lineWidth then
              emit word
              emit " "
              i <- i + word.Length + 1
              first <- false
          else
              sb.AppendLine() |> ignore
              for i in 1..indent do
                  emit " "
              emit word
              emit " "
              i <- indent + word.Length + 1
              first <- true
      sb.AppendLine() |> ignore

  /// Wrap text so it fits to a line width
  let wrapText (text : String) lineWidth =
        //dont wrap empty lines
        if text.Length = 0 then text
        else
            let sb = StringBuilder()
            let lines = text.Split [| '\r'; '\n' |]
            for line in lines do
                if line.Length <= lineWidth then sb.AppendLine(line) |> ignore
                else splitLine sb line lineWidth
            sb.ToString()

  let inline isNotNull v = not (isNull v)
  let getLines (str: string) =
      use reader = new StringReader(str)
      [|
          let line = ref (reader.ReadLine())
          while isNotNull (!line) do
              yield !line
              line := reader.ReadLine()
          if str.EndsWith("\n") then
              // last trailing space not returned
              // http://stackoverflow.com/questions/19365404/stringreader-omits-trailing-linebreak
              yield String.Empty
      |]

[<AutoOpen>]
module FSharpSymbolExt =
    open Microsoft.FSharp.Compiler.SourceCodeServices

    type FSharpSymbol with
        member x.IsSymbolLocalForProject =
            match x with
            | :? FSharpParameter -> true
            | :? FSharpMemberOrFunctionOrValue as m -> not m.IsModuleValueOrMember || not m.Accessibility.IsPublic
            | :? FSharpEntity as m -> not m.Accessibility.IsPublic
            | :? FSharpGenericParameter -> true
            | :? FSharpUnionCase as m -> not m.Accessibility.IsPublic
            | :? FSharpField as m -> not m.Accessibility.IsPublic
            | _ -> false

        member x.XmlDocSig =
            match x with
            | :? FSharpMemberOrFunctionOrValue as func -> func.XmlDocSig
            | :? FSharpEntity as fse -> fse.XmlDocSig
            | :? FSharpField as fsf -> fsf.XmlDocSig
            | :? FSharpUnionCase as fsu -> fsu.XmlDocSig
            | :? FSharpActivePatternCase as apc -> apc.XmlDocSig
            | :? FSharpGenericParameter -> ""
            | _ -> ""

    type FSharpMemberOrFunctionOrValue with
      // FullType may raise exceptions (see https://github.com/fsharp/fsharp/issues/307).
        member x.FullTypeSafe = Option.attempt (fun _ -> x.FullType)
        member x.IsConstructor = x.CompiledName = ".ctor"
        member x.IsOperatorOrActivePattern =
            let name = x.DisplayName
            if name.StartsWith "( " && name.EndsWith " )" && name.Length > 4
            then name.Substring (2, name.Length - 4) |> String.forall (fun c -> c <> ' ')
            else false
        member x.EnclosingEntitySafe =
            try
                Some x.EnclosingEntity
            with :? InvalidOperationException -> None

    type FSharpEntity with
        member x.TryGetFullName() =
            Option.attempt (fun _ -> x.TryFullName)
            |> Option.flatten
            |> Option.orTry (fun _ -> Option.attempt (fun _ -> String.Join(".", x.AccessPath, x.DisplayName)))

        member x.TryGetFullNameWithUnderScoreTypes() =
            try
                let name = String.Join(".", x.AccessPath, x.DisplayName)
                if x.GenericParameters.Count > 0 then
                  Some (name + "<" + String.concat "," (x.GenericParameters |> Seq.map (fun gp -> gp.DisplayName)) + ">")
                else Some name
            with _ -> None

        member x.UnAnnotate() =
            let rec realEntity (s:FSharpEntity) =
                if s.IsFSharpAbbreviation
                then realEntity s.AbbreviatedType.TypeDefinition
                else s
            realEntity x

        member x.InheritanceDepth() =
            let rec loop (ent:FSharpEntity) l =
                match ent.BaseType with
                | Some bt -> loop (bt.TypeDefinition.UnAnnotate()) l + 1
                | None -> l
            loop x 0
         
        //TODO: Do we need to unannotate like above?   
        member x.AllBaseTypes =
            let rec allBaseTypes (entity:FSharpEntity) =
                [
                    match entity.TryFullName with
                    | Some _ ->
                        match entity.BaseType with
                        | Some bt ->
                            yield bt
                            if bt.HasTypeDefinition then
                                yield! allBaseTypes bt.TypeDefinition
                        | _ -> ()
                    | _ -> ()
                ]
            allBaseTypes x

[<AutoOpen>]
module ConstraintExt =
    open Microsoft.FSharp.Compiler.SourceCodeServices

    type FSharpGenericParameterMemberConstraint with
        member x.IsProperty =
            (x.MemberIsStatic && x.MemberArgumentTypes.Count = 0) ||
            (not x.MemberIsStatic && x.MemberArgumentTypes.Count = 1)