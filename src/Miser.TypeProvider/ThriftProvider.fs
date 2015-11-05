namespace Miser

#nowarn "25"

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes

module internal Option = 
    let orElse v o = match o with | None -> v | Some v -> v

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
            headers |> List.filter(function AST.NamespaceHeader(ns) -> true | _ -> false) 
                    |> List.tryPick(function | AST.NamespaceHeader(AST.Namespace(AST.NamespaceScope.FSharp,_,AST.Identifier(ns))) -> Some(ns) 
                                             | AST.NamespaceHeader(AST.Namespace(AST.NamespaceScope.CSharp,_,AST.Identifier(ns))) -> Some(ns)
                                             | AST.NamespaceHeader(AST.Namespace(AST.NamespaceScope.All,_,AST.Identifier(ns))) -> Some(ns)
                                             | _ -> None)
    let getStructs (doc:AST.Document) = 
        match doc with
        | AST.Document(_,defs) ->
            defs |> List.choose(function AST.StructDef(s) -> Some s | _ -> None)
    let getExceptions (doc:AST.Document) =
        match doc with
        | AST.Document(_,defs) ->
            defs |> List.choose (function AST.ExceptionDef(e) -> Some e | _ -> None)
    let getEnums (doc:AST.Document) = 
        match doc with
        | AST.Document(_,defs) ->
            defs |> List.choose (function AST.EnumDef(e) -> Some e | _ -> None)
    let getServies (doc:AST.Document) = 
        match doc with
        | AST.Document(_,defs) ->
            defs |> List.choose (function AST.ServiceDef(s) -> Some s | _ -> None)

module internal Naming = 
    let toPascal (s:string) = 
        match s.ToCharArray() |> List.ofArray with
        | c::rest -> Char.ToUpper c::rest |> List.toArray |> String
        | _ -> s

    let toFieldName (s:string) =
        if s.StartsWith("_") then
            s
        else sprintf "_%s" s

module internal PropertyBuilder = 
    type Dictionary<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>
    let baseOption = typeof<Option<_>>.GetGenericTypeDefinition()
    let baseList = typeof<List<_>>.GetGenericTypeDefinition()
    let baseDict = typeof<System.Collections.Generic.Dictionary<_,_>>.GetGenericTypeDefinition()
    let baseSet = typeof<Set<_>>.GetGenericTypeDefinition()

    let makeOption t = baseOption.MakeGenericType([|t|])
    let makeList t = baseList.MakeGenericType([|t|])
    let makeSet t = baseSet.MakeGenericType([|t|])
    let makeMap k v = baseDict.MakeGenericType([|k;v|])
        
    let getType (typeMap:Dictionary<string,ProvidedTypeDefinition>) (ftype:ThriftAST.FieldType) (isReq:bool option) = 
        let rec buildType (t:ThriftAST.FieldType) =
            match t with
            | ThriftAST.BaseField(baseType) ->
                match baseType with
                | ThriftAST.BaseType.Binary -> typeof<byte array>
                | ThriftAST.BaseType.Bool -> typeof<bool>
                | ThriftAST.BaseType.Byte -> typeof<byte>
                | ThriftAST.BaseType.Double -> typeof<double>
                | ThriftAST.BaseType.I16 -> typeof<int16>
                | ThriftAST.BaseType.I32 -> typeof<int>
                | ThriftAST.BaseType.I64 -> typeof<int64>
                | ThriftAST.BaseType.SList -> typeof<string array>
                | ThriftAST.BaseType.String -> typeof<string>
            | ThriftAST.ContainerField(ThriftAST.ContainerType.List(t)) ->
                buildType t |> makeList
            | ThriftAST.ContainerField(ThriftAST.ContainerType.Set(t)) ->
                buildType t |> makeSet
            | ThriftAST.ContainerField(ThriftAST.ContainerType.Map(k,v)) ->
                let key = buildType k
                let value = buildType v
                makeMap key value
            | ThriftAST.IdentifierField(ThriftAST.Identifier(ident)) ->
                match typeMap.TryGetValue(ident) with
                | false,_ -> failwithf "Type: '%s' is not defined" ident
                | true,t -> upcast t
        match isReq with
        | Some true -> buildType ftype
        | _ -> makeOption (buildType ftype)
    
    let createProperty typeMap (field:ThriftAST.FieldDefinition) = 
        match field with
        | ThriftAST.Field(_,isReq,fieldType,ThriftAST.Identifier(fName),value) ->
            let field = ProvidedField(Naming.toFieldName fName,getType typeMap fieldType isReq)
            
            let prop = ProvidedProperty(Naming.toPascal fName,getType typeMap fieldType isReq)
            prop.GetterCode <- fun [this] -> Quotations.Expr.FieldGet(this,field)
            prop.SetterCode <- fun [this;value] -> Quotations.Expr.FieldSet(this,field,value)
            (fName,[field :> MemberInfo;prop :> MemberInfo])

module internal ThriftBuilder =
    
    (*
    let readStruct (iprot:Thrift.Protocol.TProtocol) = 
        let rec readFields (field:Thrift.Protocol.TField) =
            if field.Type = Thrift.Protocol.TType.Stop then
                ()
            if field.ID = 1s && field.Type = Thrift.Protocol.TType.String then Name <- iprot.ReadString()
            elif field.ID = 2s && field.Type = Thrift.Protocol.TType.I32 then Id <- iprot.ReadI32()
            elif field.ID = 3s && field.Type = Thrift.Protocol.TType.String then Description <- iprot.ReadString()
            else Thrift.Protocol.TProtocolUtil.Skip(iprot,field.Type)
            iprot.ReadFieldEnd()
            readFields (iprot.ReadFieldBegin())
        iprot.ReadStructBegin() |> ignore
        let field = iprot.ReadFieldBegin()
        readFields field
        iprot.ReadFieldEnd()
        iprot.ReadStructEnd()
    *)

    let someCaseInfo ftype = 
        let makeOpType t = 
            let op = typeof<Option<_>>.GetGenericTypeDefinition()
            op.MakeGenericType([|t|])
        let opType =
            match ftype with
            | ThriftAST.BaseField(ThriftAST.BaseType.Binary) -> makeOpType typeof<byte array>
            | ThriftAST.BaseField(ThriftAST.BaseType.Bool) -> makeOpType typeof<bool>
            | ThriftAST.BaseField(ThriftAST.BaseType.Byte) -> makeOpType typeof<byte>
            | ThriftAST.BaseField(ThriftAST.BaseType.Double) -> makeOpType typeof<double>
            | ThriftAST.BaseField(ThriftAST.BaseType.I16) -> makeOpType typeof<int16>
            | ThriftAST.BaseField(ThriftAST.BaseType.I32) -> makeOpType typeof<int>
            | ThriftAST.BaseField(ThriftAST.BaseType.I64) -> makeOpType typeof<int64>
            | ThriftAST.BaseField(ThriftAST.BaseType.SList) -> makeOpType typeof<string list>
            | ThriftAST.BaseField(ThriftAST.BaseType.String) -> makeOpType typeof<string>
        let ctor = opType.GetMethods(BindingFlags.Public ||| BindingFlags.Static) |> Array.find (fun mi -> mi.Name = "Some")
        ctor
        

    let normalizeFieldIds (fields:ThriftAST.FieldDefinition list) = 
        fields 
        |> List.fold (fun (counter,fields) field -> 
            match field with
            | ThriftAST.Field(Some id,_,_,_,_) as f -> (id+1L,f::fields)
            | ThriftAST.Field(None,isReq,fieldType,ident,value) -> (counter + 1L,ThriftAST.Field(Some counter,isReq,fieldType,ident,value)::fields) )(1L,[])
 
    let getFieldType (f:ThriftAST.FieldType) = 
        match f with
        | ThriftAST.BaseField(ThriftAST.BaseType.Binary) -> <@ Thrift.Protocol.TType.Byte @>
        | ThriftAST.BaseField(ThriftAST.BaseType.Bool) -> <@ Thrift.Protocol.TType.Bool @>
        | ThriftAST.BaseField(ThriftAST.BaseType.Byte) -> <@ Thrift.Protocol.TType.Byte @>
        | ThriftAST.BaseField(ThriftAST.BaseType.Double) -> <@ Thrift.Protocol.TType.Double @>
        | ThriftAST.BaseField(ThriftAST.BaseType.I16) -> <@ Thrift.Protocol.TType.I16 @>
        | ThriftAST.BaseField(ThriftAST.BaseType.I32) -> <@ Thrift.Protocol.TType.I32 @>
        | ThriftAST.BaseField(ThriftAST.BaseType.I64) -> <@ Thrift.Protocol.TType.I64 @>
        | ThriftAST.BaseField(ThriftAST.BaseType.SList) -> <@ Thrift.Protocol.TType.List @>
        | ThriftAST.BaseField(ThriftAST.BaseType.String) -> <@ Thrift.Protocol.TType.String @>
        | ThriftAST.IdentifierField _ -> <@ Thrift.Protocol.TType.Struct @>
        | ThriftAST.ContainerField(ThriftAST.ContainerType.List(_)) -> <@ Thrift.Protocol.TType.List @>
        | ThriftAST.ContainerField(ThriftAST.ContainerType.Map(_)) -> <@ Thrift.Protocol.TType.Map @>
        | ThriftAST.ContainerField(ThriftAST.ContainerType.Set(_)) -> <@ Thrift.Protocol.TType.Set @>

    let getReaderMethod (isReq) (f:ThriftAST.FieldType) (iprot:Expr<Thrift.Protocol.TProtocol>):Expr = 
        let getVal = 
            match f with
            | ThriftAST.BaseField(ThriftAST.BaseType.Binary) -> <@@ (%iprot).ReadBinary() @@>
            | ThriftAST.BaseField(ThriftAST.BaseType.Bool) -> <@@ (%iprot).ReadBool() @@>
            | ThriftAST.BaseField(ThriftAST.BaseType.Byte) -> <@@ (%iprot).ReadByte() @@>
            | ThriftAST.BaseField(ThriftAST.BaseType.Double) -> <@@ (%iprot).ReadDouble() @@>
            | ThriftAST.BaseField(ThriftAST.BaseType.I16) -> <@@ (%iprot).ReadI16() @@>
            | ThriftAST.BaseField(ThriftAST.BaseType.I32) -> <@@ (%iprot).ReadI32() @@>
            | ThriftAST.BaseField(ThriftAST.BaseType.I64) -> <@@ (%iprot).ReadI64() @@>
            | ThriftAST.BaseField(ThriftAST.BaseType.SList) -> <@@ (%iprot).ReadString() @@>
            | ThriftAST.BaseField(ThriftAST.BaseType.String) -> <@@ (%iprot).ReadString() @@>
            | ThriftAST.IdentifierField _ -> <@@ (%iprot).ReadString() @@>
            | ThriftAST.ContainerField(ThriftAST.ContainerType.List(_)) -> <@@ (%iprot).ReadListBegin() @@>
            | ThriftAST.ContainerField(ThriftAST.ContainerType.Map(_)) -> <@@ (%iprot).ReadMapBegin() @@>
            | ThriftAST.ContainerField(ThriftAST.ContainerType.Set(_)) -> <@@ (%iprot).ReadSetBegin() @@>
        match isReq with
        | Some(true) -> getVal
        | _ -> Expr.Call(someCaseInfo f,[getVal])

    let buildReaderForField ( this:Expr) (iprot:Expr<Thrift.Protocol.TProtocol>) (fieldVar:Expr<Thrift.Protocol.TField>) fieldInfo (ThriftAST.Field(Some(fieldId),isReq,fieldType,_,_)) =
            let reader = getReaderMethod isReq fieldType iprot
            let setField = Expr.PropertySet(this,fieldInfo,reader)
            let targetType = getFieldType fieldType
            let fieldID:Expr<int16> = Expr.Value(int16 fieldId) |> Expr.Cast

            let checkField = <@ (%fieldVar).ID = (%fieldID) && (%fieldVar).Type = %targetType @>
            checkField,setField

    let buildReader (fields:ThriftAST.FieldDefinition list) = 
        normalizeFieldIds fields
         
    let createReaderForStruct (fieldNfos:Map<string,ProvidedProperty>)(s:ThriftAST.StructDefinition) = 
        let (ThriftAST.Struct(_,fields)) = s
        
        fun (args:Expr list) ->
            let iprot:Expr<Thrift.Protocol.TProtocol> = args.[1] |> Expr.Cast
            let this = args.[0]
            let fieldVar = Var("field",typeof<Thrift.Protocol.TField>) 
            let fieldVarExpr = Expr.Var(fieldVar) |> Expr.Cast
            let buildField = buildReaderForField this iprot (fieldVarExpr)
            let fields = fields |> List.choose (fun f ->
                            let (ThriftAST.Field(_,_,_,ThriftAST.Identifier(name),_)) = f
                            match Map.tryFind name fieldNfos with
                            | None -> None
                            | Some fNfo -> Some (buildField fNfo f))
            let apply = 
                List.foldBack (fun (checkField,setField) x -> Expr.IfThenElse(checkField,setField,x)) fields <@ Thrift.Protocol.TProtocolUtil.Skip(%iprot,(%fieldVarExpr).Type) @>.Raw
            let applyFunc:Expr<Thrift.Protocol.TField -> unit> = Expr.Lambda(fieldVar,apply) |> Expr.Cast
            <@
                let iprot:Thrift.Protocol.TProtocol = (%%(args.[1]):Thrift.Protocol.TProtocol)
                let rec readField (field:Thrift.Protocol.TField) = 
                    if field.Type = Thrift.Protocol.TType.Stop then ignore()
                    else
                        (%applyFunc) field
                        readField (iprot.ReadFieldBegin())
                    
                iprot.ReadStructBegin() |> ignore
                readField (iprot.ReadFieldBegin())
                iprot.ReadStructEnd()
            @>.Raw
    let createWriterForStruct (s:ThriftAST.StructDefinition) = 
        let name = let (ThriftAST.Struct(ThriftAST.Identifier(sname),_)) = s in sname
        fun (args:Expr list) ->
            <@@
                let oprot:Thrift.Protocol.TProtocol = (%%(args.[1]):Thrift.Protocol.TProtocol)
                oprot.WriteStructEnd()
            @@>

    let private baseReader = typeof<Thrift.Protocol.TBase>.GetMethod "Read"
    let private baseWriter = typeof<Thrift.Protocol.TBase>.GetMethod "Write"
        
    let createReader fieldInfo s = 
        ProvidedMethod("Read",[ProvidedParameter("tProtocol",typeof<Thrift.Protocol.TProtocol>)],typeof<Void>,
                                        InvokeCode = createReaderForStruct fieldInfo s)
    
    let createWriter s = 
        ProvidedMethod("Write",[ProvidedParameter("tProtocol",typeof<Thrift.Protocol.TProtocol>)],typeof<Void>,
                                        InvokeCode = createWriterForStruct s)
        

    let buildProperties typeMap (t:ProvidedTypeDefinition) (fields) = 
        let fieldMap = 
            fields 
            |> List.map (PropertyBuilder.createProperty typeMap)
            |> List.fold (fun fieldMap (name,[field;property]) ->
                t.AddMember field
                t.AddMember property
                fieldMap |> Map.add name (property :?> ProvidedProperty)) Map.empty
        fieldMap
    let private buildObject typeMap baseType name s = 
        let t = ProvidedTypeDefinition(Naming.toPascal name,Some baseType,IsErased = false)
        let fieldMap = buildProperties typeMap t s
        let ctor = ProvidedConstructor([])
        ctor.InvokeCode <- (fun args -> <@@ ignore() @@>)
        t.AddMember ctor
        fieldMap,t
    let buildException typeMap name (ThriftAST.Exception(_,s)) = 
        let fieldMap,t = buildObject typeMap typeof<Exception> name s
        name,t

    let buildUnion typeMap name (ThriftAST.Union(_,s)) =
        let fieldMap,t = buildObject typeMap typeof<obj> name s
        name,t

    let buildStruct typeMap name (ThriftAST.Struct(_,s) as structObj) = 
        let fieldMap,t = buildObject typeMap typeof<obj> name s
        let reader = createReader fieldMap structObj
        t.AddMember reader
        name,t

type internal StructCompiler(root:ProvidedTypeDefinition,tdoc) =
    let items = match tdoc with ThriftAST.Document(_,items) -> items
    let compiledTypes = System.Collections.Generic.Dictionary<_,_>()
    member __.Compile() =
        items |> List.toSeq
              |> Seq.choose (function 
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
    let thisAssembly = Assembly.GetExecutingAssembly()
    let rootNamespace = "Miser"
    let parameters = [ProvidedStaticParameter("path",typeof<string>)]
    let t = ProvidedTypeDefinition(thisAssembly,rootNamespace,"Thrift",None,IsErased = false)
    let tempAsmPath = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),Path.GetFileName(IO.Path.ChangeExtension(IO.Path.GetTempFileName(), ".dll")))
    do printfn "Assembly: %s" tempAsmPath
    let tempAsm = ProvidedAssembly tempAsmPath
    do t.DefineStaticParameters(parameters,apply = 
        fun typeName parameterValues ->
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
            let root = ProvidedTypeDefinition(thisAssembly,namespaceName,typeName,Some typeof<obj>,IsErased = false)
            root.AddXmlDoc("Miser Provider for " + path)
            let s= StructCompiler(root,thriftDoc).Compile()
            tempAsm.AddTypes [root]
            root)
    do 
        tempAsm.AddTypes [t]
        this.RegisterRuntimeAssemblyLocationAsProbingFolder config
        this.AddNamespace("Miser",[t])


[<assembly:TypeProviderAssembly>]
do()
