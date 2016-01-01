namespace Miser

open Util
open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes

module internal ThriftHandler =
    open FParsec 
    module AST = ThriftAST

    let loadFile path = 
        let fileText = File.ReadAllText(path)
        match run ThriftParser.document fileText with
        | Success(doc,_,_) -> doc
        | Failure(err,_,_) -> failwith err

    let getNamespace (doc:AST.Document) =
        match doc with
        | AST.Document(headers,_) ->
            headers |> List.filter(function AST.NamespaceHeader(_) -> true | _ -> false) 
                    |> List.tryPick(function | AST.NamespaceHeader(AST.Namespace(AST.NamespaceScope.FSharp,_,AST.Identifier(ns))) -> Some(ns) 
                                             | AST.NamespaceHeader(AST.Namespace(AST.NamespaceScope.CSharp,_,AST.Identifier(ns))) -> Some(ns)
                                             | AST.NamespaceHeader(AST.Namespace(AST.NamespaceScope.All,_,AST.Identifier(ns))) -> Some(ns)
                                             | _ -> None)

open ThriftReader
open ThriftWriter
open ThriftHelpers

module internal ThriftBuilder =
    open PropertyBuilder

    type ThriftConfig = {
        generateLenses:bool
        generateAsync:bool 
        useOptions:bool }

    let defaultConfig = 
        { generateLenses = false
          generateAsync = false
          useOptions = true }
    
    let createReader typeMap (fieldInfo:FieldMap) s = 
        ProvidedMethod("Read",
                       [ProvidedParameter("tProtocol",typeof<Thrift.Protocol.TProtocol>)],
                       typeof<Void>,
                       InvokeCode = ((createReaderForStruct typeMap fieldInfo s) >> Expr.raw))
    
    let createWriter typeMap fieldInfo s = 
        ProvidedMethod("Write",
                       [ProvidedParameter("tProtocol",typeof<Thrift.Protocol.TProtocol>)],
                       typeof<Void>,
                       InvokeCode = ((createWriterForStruct typeMap fieldInfo s) >> Expr.raw))
    
    let buildFields typeMap (t:ProvidedTypeDefinition) (fields) = 
        fields
        |> List.map (PropertyBuilder.createField typeMap)    
        |> List.fold (fun map field ->
            t.AddMember field.field
            Map.add field.originalName field map) Map.empty
       
    let buildConstructor (t:ProvidedTypeDefinition) = 
        t.AddMemberDelayed (fun () -> ProvidedConstructor([],InvokeCode = fun _ -> <@@ () @@>))
        t

    let buildReader typeMap (fieldMap:FieldMap) name s (t:ProvidedTypeDefinition) = 
        let reader = createReader typeMap fieldMap (name,s)
        reader.SetMethodAttrs(MethodAttributes.Virtual)
        t.AddInterfaceImplementation(typeof<Thrift.Protocol.TBase>)
        let tbaseRead = typeof<Thrift.Protocol.TBase>.GetMethods() |> Array.head
        t.DefineMethodOverride(reader,tbaseRead)
        t.AddMember reader
        t

    let buildWriter typeMap fieldMap name s (t:ProvidedTypeDefinition) = 
        let writer = createWriter typeMap fieldMap (name,s)
        writer.SetMethodAttrs(MethodAttributes.Virtual)
        t.AddInterfaceImplementation(typeof<Thrift.Protocol.TAbstractBase>)
        let tbaseWrite = typeof<Thrift.Protocol.TAbstractBase>.GetMethods() |> Array.head
        t.DefineMethodOverride(writer,tbaseWrite)
        t.AddMember writer
        t

    let private buildObject typeMap baseType name s = 
        let t = ProvidedTypeDefinition(Naming.toPascal name,Some baseType,IsErased = false) |> buildConstructor
        let fieldMap = buildFields typeMap t s 
        //buildReader typeMap fieldMap name s t
        buildWriter typeMap fieldMap name s t
        
    let buildException typeMap name (ThriftAST.Exception(_,fields)) = 
        let t = buildObject typeMap typeof<Exception> name fields
        name,t

    let buildUnion typeMap name (ThriftAST.Union(_,fields)) =
        let t = buildObject typeMap typeof<obj> name fields
        name,t

    let buildStruct typeMap name (ThriftAST.Struct(_,fields)) = 
        let t = buildObject typeMap typeof<obj> name fields
        name,t

    let buildEnum name fields = 
        let t = ProvidedTypeDefinition(name,Some typeof<Enum>,IsErased = false)
        t.SetEnumUnderlyingType typeof<int>
        fields |> List.fold (fun (i,values) (ThriftAST.Identifier(id),value) ->
            let v = 
                match value with
                | None -> int i
                | Some v -> int v
            (int64 v + 1L),ProvidedLiteralField(id,typeof<int>,box v)::values) (1L,[])
        |> snd 
        |> t.AddMembers
        name,t

type internal StructCompiler(root:ProvidedTypeDefinition,tdoc) =
    let items = match tdoc with ThriftAST.Document(_,items) -> items
    let compiledTypes = System.Collections.Generic.Dictionary<_,_>()
    member __.Compile() =
        items |> List.toSeq
              |> Seq.choose (function 
                                | ThriftAST.EnumDef(ThriftAST.Enum(ThriftAST.Identifier(name),fields)) -> ThriftBuilder.buildEnum name fields |> Some
                                | ThriftAST.StructDef(ThriftAST.Struct(ThriftAST.Identifier(name),_) as s) -> ThriftBuilder.buildStruct compiledTypes name s |> Some
                                | ThriftAST.ExceptionDef(ThriftAST.Exception(ThriftAST.Identifier(name),_) as e) -> ThriftBuilder.buildException compiledTypes name e |> Some
                                | ThriftAST.UnionDef(ThriftAST.Union(ThriftAST.Identifier(name),_) as u) -> ThriftBuilder.buildUnion compiledTypes name u |> Some
                                | _ -> None)
              |> Seq.iter(fun (name,item) -> 
                            compiledTypes.Add(name,item)
                            root.AddMember item)
        root

[<TypeProvider>]
type public Provider(config:TypeProviderConfig) as this = 
    inherit TypeProviderForNamespaces()
    let thisAssembly = Assembly.LoadFrom(config.RuntimeAssembly)
    let rootNamespace = "Miser"
    let parameters = [ProvidedStaticParameter("path",typeof<string>)]
    let t = ProvidedTypeDefinition(thisAssembly,rootNamespace,"Thrift",None,IsErased = false)
    let tempAsmPath = Path.Combine(
                        Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                        Path.GetFileName(IO.Path.ChangeExtension(IO.Path.GetTempFileName(), ".dll")))
    do printfn "Assembly: %s" tempAsmPath
    let tempAsm = ProvidedAssembly tempAsmPath
    let applyParameters typeName (parameterValues:obj []) = 
        let path = 
            match parameterValues with
            | [| :? string as path |] -> path
            | _ -> failwith "A Path to a thrift definition file is required"
        let thriftDoc = 
                if (File.Exists(path)) then
                    ThriftHandler.loadFile path
                elif (File.Exists(path+".thrift")) then
                    ThriftHandler.loadFile (path+".thrift")
                else failwithf "Unable to find file: %s" path
        let rootName = Path.GetFileNameWithoutExtension(path)
        let namespaceName = ThriftHandler.getNamespace thriftDoc |> Option.orElse (rootName)
        let root = ProvidedTypeDefinition(thisAssembly,
                                          namespaceName,
                                          typeName,
                                          Some typeof<obj>,
                                          IsErased = false)
        root.AddXmlDoc("Miser Provider for " + path)
        let _ = StructCompiler(root,thriftDoc).Compile()
        tempAsm.AddTypes [root]
        root
    do t.DefineStaticParameters(parameters,apply = applyParameters)
    do 
        tempAsm.AddTypes [t]
        this.RegisterRuntimeAssemblyLocationAsProbingFolder config
        this.AddNamespace("Miser",[t])


[<assembly:TypeProviderAssembly>]
do()