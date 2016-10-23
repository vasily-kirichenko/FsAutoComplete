namespace FsAutoComplete

open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

module ProjectCoreCracker = 
  let GetProjectOptionsFromProjectFile (file : string)  = 
    let projDir = Path.GetDirectoryName file
    let options =  
      Directory.GetFiles(projDir, "dotnet-compile-fsc.rsp", SearchOption.AllDirectories) 
      |> Seq.head
      |> File.ReadAllLines
      |> Array.map (fun s -> 
           if s.EndsWith ".fs" then 
              let p = Path.GetFullPath s
              (p.Chars 0).ToString().ToLower() + p.Substring(1)
           else s )
      |> Array.filter((<>) "--nocopyfsharpcore") 
      
    let fileNames, otherOptions =
        options |> Array.partition (fun x -> x.EndsWith ".fs")

    let sepChar = Path.DirectorySeparatorChar
            
    let fileNames = fileNames |> Array.map (fun x -> 
       match sepChar with
       | '\\' -> x.Replace('/', '\\')
       | '/' -> x.Replace('\\', '/')
       | _ -> x
    )

    { 
      ProjectFileName = file
      ProjectFileNames = fileNames
      OtherOptions = Array.append otherOptions fileNames
      ReferencedProjects = [||]
      IsIncompleteTypeCheckEnvironment = false
      UseScriptResolutionRules = false
      LoadTime = DateTime.Now
      UnresolvedReferences = None;
    }