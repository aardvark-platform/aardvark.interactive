namespace Aardvark.Interactive

open System
open System.Threading
open System.Threading.Tasks
open Microsoft.DotNet.Interactive
open Microsoft.DotNet.Interactive.Formatting
open Microsoft.DotNet.Interactive.FSharp
open Microsoft.DotNet.Interactive.Commands
open Adaptify.Compiler
open System.Reflection
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Scripting

module AdaptifyCustom =
    open FSharp.Compiler.Range
    
    let getReplacementCode (log : ILog) (createLenses : bool) (res : FSharpCheckFileResults) (code : string) =

        let errs, wrns = res.Errors |> Array.partition (fun err -> err.Severity = FSharpErrorSeverity.Error)
        if errs.Length > 0 then
            let errorStrings =
                errs |> Seq.map (fun err ->
                    sprintf "  (%d,%d): %s" err.StartLineAlternate err.StartColumn err.Message
                )
                |> String.concat "\r\n"

            let range = mkRange "internal" pos0 pos0
            log.warn range "internal" "compiler errors:\r\n%s" errorStrings

        for err in wrns do
            let p0 = mkPos err.StartLineAlternate err.StartColumn
            let p1 = mkPos err.EndLineAlternate err.EndColumn
            let range = mkRange err.FileName p0 p1
            log.warn range (sprintf "%04d" err.ErrorNumber) "%s" err.Message

        let rec allEntities (d : FSharpImplementationFileDeclaration) =
            match d with
            | FSharpImplementationFileDeclaration.Entity(e, ds) ->
                e :: List.collect allEntities ds
            | _ ->
                []

        let entities =
           
            res.ImplementationFile.Value.Declarations
            |> Seq.toList
            |> List.collect allEntities
            
        let definitions =   
            entities 
            |> List.choose (TypeDef.ofEntity log)
            |> List.map (fun l -> l.Value)
            |> List.collect (TypeDefinition.ofTypeDef log createLenses [])

        match definitions with
        | [] ->
            (0,0), code
        | _ -> 
            let defs = definitions |> List.toArray |> Array.collect TypeDefinition.toString

            let newCode = 
                String.concat "\r\n" [
                    "#nowarn \"49\" // upper case patterns"
                    "#nowarn \"66\" // upcast is unncecessary"
                    "#nowarn \"1337\" // internal types"
                    "[<AutoOpen>]"
                    "module rec Adaptify ="
                    "    open FSharp.Data.Adaptive"
                    "    open Adaptify"
                    indentStr code
                    yield! indent defs
                ]

            (7, 4), newCode

            

type AdaptifyExtension() =

    static let all = BindingFlags.NonPublic ||| BindingFlags.Instance
    static let fScript = typeof<FSharpKernel>.GetField("script", all)
    static let fKeepAssemblyContents = typeof<FSharpCheckFileResults>.GetField("keepAssemblyContents", all)

    interface IKernelExtension with
        member x.OnLoadAsync(kernel) =
            System.Diagnostics.Debugger.Launch() |> ignore

            if not (isNull KernelInvocationContext.Current) then
                KernelInvocationContext.Current.Display("boot") |> ignore

            

            kernel.AddMiddleware(KernelCommandPipelineMiddleware(fun cmd ctx cont ->
                match ctx.HandlingKernel with
                | :? FSharpKernel as kernel -> 
                    
                    let offset (l : int, c : int) (p : Microsoft.CodeAnalysis.Text.LinePosition) =
                        Microsoft.CodeAnalysis.Text.LinePosition(p.Line + l, p.Character + c)

                    let codeAndUpdate = 
                        match cmd with
                        | :? SubmitCode as cmd ->
                            ctx.Display("SubmitCode") |> ignore
                            Some (cmd.Code, fun c off -> SubmitCode(c, cmd.TargetKernelName, cmd.SubmissionType) :> KernelCommand)
                        | :? RequestCompletions as cmd ->
                            ctx.Display("RequestCompletions") |> ignore
                            Some (cmd.Code, fun c off -> RequestCompletions(c, offset off cmd.LinePosition, cmd.TargetKernelName) :> KernelCommand)
                        | :? RequestHoverText as cmd ->
                            ctx.Display("RequestHoverText") |> ignore
                            Some (cmd.Code, fun c off -> RequestHoverText(c, offset off cmd.LinePosition, cmd.TargetKernelName) :> KernelCommand)
                        | :? RequestDiagnostics as cmd ->
                            ctx.Display("RequestDiagnostics") |> ignore
                            Some (cmd.Code, fun c off -> RequestDiagnostics(c, cmd.TargetKernelName) :> KernelCommand)
                        | _ ->
                            ctx.Display(cmd.GetType().FullName) |> ignore
                            None

                    match codeAndUpdate with
                    | Some (code, update) ->
                        // hack to get access to `FsiEvaluationSession`
                        let script = fScript.GetValue(kernel) |> unbox<Lazy<FSharpScript>>
                        let (_parse, check, _proj) = script.Value.Fsi.ParseAndCheckInteraction(code) |> Async.RunSynchronously

                        // hack to make `ImplementationFile` accessible
                        fKeepAssemblyContents.SetValue(check, true)

                        let off, newCode = AdaptifyCustom.getReplacementCode Log.empty false check code
                        cont.Invoke(update newCode off, ctx)

                    | None ->
                        cont.Invoke(cmd, ctx)
                | _ ->
                    cont.Invoke(cmd, ctx)
            ))

            
            Task.FromResult() :> Task


