namespace FsAutoComplete

open System
open System.IO
open System.Text
open System.Collections.Generic
open System.Linq
open System.Xml
open ExtCore.Control
open System.Xml.Linq

[<RequireQualifiedAccess>]
type Style = 
    | Type of string
    | Parameter of string
    | Code of string
    | Exception of string

module Styles = 
    let simpleMarkup style = 
        match style with
        | Style.Type name -> String.Format("`{0}` ", name)
        | Style.Parameter name -> String.Format("`{0}` ", name)
        | Style.Code name -> String.Format("```{0}``` ", name)
        | Style.Exception name -> String.Format("\n`{0}`", name)

module GLib = 
    module Markup =
        let EscapeText (s: string) = s

module Linq2Xml = 
    let xn = XName.op_Implicit
    let xs ns local = XName.Get(local, ns)
    let firstOrDefault seq = Enumerable.FirstOrDefault seq
    
    let firstOrNone seq = 
        let iter = Enumerable.FirstOrDefault seq
        match iter with
        | null -> None
        | _ -> Some iter
    
    let singleOrDefault seq = Enumerable.SingleOrDefault(seq)
    let where (pred : XElement -> bool) elements = Enumerable.Where(elements, pred)
    let attribute name (element : XElement) = element.Attribute <| xn name
    let attributeValue name element = (attribute name element).Value
    let descendants xs (element : XElement) = element.Descendants xs
    
    let previousNodeOrNone (element : XElement) = 
        match element.PreviousNode with
        | null -> None
        | node -> Some node

module TooltipsXml = 
    open Linq2Xml
    
    let private strip start (str : string) = 
        if str.StartsWith start then str.Substring(start.Length)
        else str
    
    let private trim (str : String) = 
        str.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> s.Trim())
        |> String.concat (" ")


    
    let private unqualifyName (txt : String) = txt.Substring(txt.LastIndexOf(".") + 1)
    
    let private elementValue (style : Style -> string) (element : XElement) = 
        let sb = StringBuilder()
        if element = null then sb
        else 
            let rec processNodes (sb : StringBuilder) (nodes : IEnumerable<XNode>) = 
                nodes.Aggregate(
                    sb, 
                    fun acc node -> 
                        match node with
                        | null -> acc
                        | :? XElement as element -> 
                            match element.Name.LocalName with
                            | "para" -> processNodes acc (element.Nodes())
                            | "see" -> 
                                match element |> attribute "cref" with
                                | null -> acc
                                | attrib -> 
                                    let fragment = 
                                        attrib.Value |> (strip "T:"
                                                         >> unqualifyName
                                                         >> Style.Type
                                                         >> style)
                                    acc.Append(fragment)
                            | "paramref" -> 
                                match element |> attribute "name" with
                                | null -> acc
                                | attrib -> 
                                    let fragment = attrib.Value |> (Style.Parameter >> style)
                                    acc.Append(fragment)
                            | "c" -> 
                                let fragment = 
                                    element.Value |> (GLib.Markup.EscapeText
                                                      >> Style.Code
                                                      >> style)
                                acc.Append(fragment)
                            | "attribution" -> acc //skip attribution elements
                            | _unknown -> 
                                //LoggingService.LogError("Error in Tooltip parsing, unknown element in summary: " + unknown)
                                processNodes acc (element.Nodes())
                        | :? XText as xt -> 
                            acc.AppendFormat("{0} ", xt.Value |> (GLib.Markup.EscapeText >> trim))
                        | _ -> acc)
            processNodes sb (element.Nodes())
    
    let getTooltipSummary (style : Style -> string) (str : string) = 
        try 
            let xdoc = 
                if str.StartsWith("<?xml") then XElement.Parse(str)
                else XElement.Parse("<Root>" + str + "</Root>")
            
            //if no nodes were found then return the string verbatim
            let anyNodes = xdoc.Descendants() |> Enumerable.Any
            if not anyNodes then str
            else 
                let summary = 
                    xdoc.Descendants(xn "summary")
                    |> firstOrDefault
                    |> elementValue style
                xdoc.Elements(xn "exception") 
                |> Seq.iteri (fun i element -> 
                       if i = 0 then summary.Append("\n\n**Exceptions**\n") |> ignore
                       match element |> attribute "cref" with
                       | null -> ()
                       | cref -> 
                           let exceptionType = 
                               cref.Value |> (strip "T:"
                                              >> unqualifyName
                                              >> Style.Exception
                                              >> style)
                           if i > 0 then summary.AppendLine() |> ignore
                           summary.AppendFormat("{0}: {1}", exceptionType, element.Value) |> ignore)
                summary.ToString().TrimEnd()
        //if the summary cannot be parsed just escape the text
        with _ -> GLib.Markup.EscapeText str
    
    let getParameterTip (addStyle : Style -> string) (str : String) (param : String) = 
        let xdoc = 
            if str.StartsWith("<?xml") then XElement.Parse(str)
            else XElement.Parse("<Root>" + str + "</Root>")
        
        let par = 
            xdoc.Descendants(xn "param")
            |> where (fun element -> (element |> attribute "name").Value = param)
            |> singleOrDefault
        
        if par = null then None
        else Some((elementValue addStyle par).ToString())

module TooltipXmlDoc = 
    ///lru based memoize
    let private memoize f n = 
        let lru = ref (ExtCore.Caching.LruCache.create n)
        fun x -> 
            match (!lru).TryFind x with
            | Some entry, cache -> 
                lru := cache
                entry
            | None, cache -> 
                let res = f x
                lru := cache.Add(x, res)
                res
    
    /// Memoize the objects that manage access to XML files, keeping only 20 most used
    // @todo consider if this needs to be a weak table in some way
    let private xmlDocProvider = 
        memoize (fun x -> 
            try 
                Some(ICSharpCode.NRefactory.Documentation.XmlDocumentationProvider(x))
            with _ -> None) 20u
    
    let private tryExt file ext = Option.condition File.Exists (Path.ChangeExtension(file, ext))
    
    /// Return the XmlDocumentationProvider for an assembly
    let findXmlDocProviderForAssembly file = 
        maybe { 
            let! xmlFile = Option.coalesce (tryExt file "xml") (tryExt file "XML")
            return! xmlDocProvider xmlFile 
        }
    
    let private findXmlDocProviderForEntity (file, key : string) = 
        maybe { 
            let! docReader = findXmlDocProviderForAssembly file
            let doc = docReader.GetDocumentation key
            if String.IsNullOrEmpty doc then return! None
            else return doc
        }
    
    /// Find the documentation for a file/key pair representing an entity with documentation
    let findDocForEntity (file, key) = findXmlDocProviderForEntity (file, key)

/// Formatting of TooltipElement information displayed in tooltips and autocompletion
module TooltipFormatting = 
    open Microsoft.FSharp.Compiler.SourceCodeServices
    
    /// Format some of the data returned by the F# compiler
    let private buildFormatComment cmt = 
        match cmt with
        | FSharpXmlDoc.Text(s) -> TooltipsXml.getTooltipSummary Styles.simpleMarkup <| s.Trim()
        | FSharpXmlDoc.XmlDocFileSignature(file, key) -> 
            match TooltipXmlDoc.findDocForEntity (file, key) with
            | None -> ""
            | Some doc -> TooltipsXml.getTooltipSummary Styles.simpleMarkup doc
        | _ -> ""
    
    /// Format some of the data returned by the F# compiler
    let private buildFormatElement el = 
        let signatureB, commentB = StringBuilder(), StringBuilder()
        match el with
        | FSharpToolTipElement.None -> ()
        | FSharpToolTipElement.Single(it, comment) -> 
            signatureB.Append(GLib.Markup.EscapeText(it)) |> ignore
            let html = buildFormatComment comment
            if not (String.IsNullOrWhiteSpace html) then commentB.Append(html) |> ignore
        | FSharpToolTipElement.Group(items) -> 
            let items, msg = 
                if items.Length > 10 then 
                    (items
                     |> Seq.take 10
                     |> List.ofSeq), sprintf "   _(+%d other overloads)_" (items.Length - 10)
                else items, null
            if (items.Length > 1) then signatureB.AppendLine("Multiple overloads") |> ignore
            items |> Seq.iteri (fun i (it, comment) -> 
                         signatureB.Append(GLib.Markup.EscapeText(it)) |> ignore
                         if i = 0 then 
                             let html = buildFormatComment comment
                             if not (String.IsNullOrWhiteSpace html) then 
                                 commentB.AppendLine(html) |> ignore
                                 commentB.Append(GLib.Markup.EscapeText "\n") |> ignore)
            if msg <> null then signatureB.Append(msg) |> ignore
        | FSharpToolTipElement.CompositionError(err) -> 
            signatureB.Append("Composition error: " + GLib.Markup.EscapeText(err)) |> ignore
        signatureB.ToString().Trim(), commentB.ToString().Trim()
    
    /// Format tool-tip that we get from the language service as string
    let formatTip (FSharpToolTipText(list)) = 
        [ for item in list -> 
              let signature, summary = buildFormatElement item
              signature, summary ]
    
    /// For elements with XML docs, the parameter descriptions are buried in the XML. Fetch it.
    let private extractParamTipFromComment paramName comment = 
        match comment with
        | FSharpXmlDoc.Text(s) -> TooltipsXml.getParameterTip Styles.simpleMarkup s paramName
        // For 'FSharpXmlDoc.XmlDocFileSignature' we can get documentation from 'xml' files, and via MonoDoc on Mono
        | FSharpXmlDoc.XmlDocFileSignature(file, key) -> 
            maybe { 
                let! docReader = TooltipXmlDoc.findXmlDocProviderForAssembly file
                let doc = docReader.GetDocumentation(key)
                if String.IsNullOrEmpty doc then return! None
                else 
                    let parameterTip = TooltipsXml.getParameterTip Styles.simpleMarkup doc paramName
                    return! parameterTip
            }
        | _ -> None
    
    /// For elements with XML docs, the parameter descriptions are buried in the XML. Fetch it.
    let private extractParamTipFromElement paramName element = 
        match element with
        | FSharpToolTipElement.None -> None
        | FSharpToolTipElement.Single(_it, comment) -> extractParamTipFromComment paramName comment
        | FSharpToolTipElement.Group items -> List.tryPick (snd >> extractParamTipFromComment paramName) items
        | FSharpToolTipElement.CompositionError _err -> None
    
    /// For elements with XML docs, the parameter descriptions are buried in the XML. Fetch it.
    let extractParamTip paramName (FSharpToolTipText elements) = 
        List.tryPick (extractParamTipFromElement paramName) elements
