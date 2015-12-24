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

module internal Expr = 
    open FSharp.Quotations
    let raw (expr:Expr<_>) = expr.Raw

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
        | ThriftAST.Field(_,isReq,fieldType,ThriftAST.Identifier(fName),_) ->
            let field = ProvidedField(Naming.toFieldName fName,getType typeMap fieldType isReq)
            
            let prop = ProvidedProperty(Naming.toPascal fName,getType typeMap fieldType isReq)
            prop.GetterCode <- fun [this] -> Quotations.Expr.FieldGet(this,field)
            prop.SetterCode <- fun [this;value] -> Quotations.Expr.FieldSet(this,field,value)
            (fName,[field :> MemberInfo;prop :> MemberInfo])

module internal ThriftBuilder =

    type ThriftConfig = {
        generateLenses:bool
        generateAsync:bool 
        useOptions:bool }

    type BaseEnum =
        | Default = 0

    let defaultConfig = 
        { generateLenses = false
          generateAsync = false
          useOptions = true }
    
    let private makeOpType t = 
        let op = typeof<Option<_>>.GetGenericTypeDefinition()
        op.MakeGenericType([|t|])
    
    let private toOpType t =
        match t with
            | ThriftAST.BaseField(ThriftAST.BaseType.Binary) -> makeOpType typeof<byte array>
            | ThriftAST.BaseField(ThriftAST.BaseType.Bool) -> makeOpType typeof<bool>
            | ThriftAST.BaseField(ThriftAST.BaseType.Byte) -> makeOpType typeof<byte>
            | ThriftAST.BaseField(ThriftAST.BaseType.Double) -> makeOpType typeof<double>
            | ThriftAST.BaseField(ThriftAST.BaseType.I16) -> makeOpType typeof<int16>
            | ThriftAST.BaseField(ThriftAST.BaseType.I32) -> makeOpType typeof<int>
            | ThriftAST.BaseField(ThriftAST.BaseType.I64) -> makeOpType typeof<int64>
            | ThriftAST.BaseField(ThriftAST.BaseType.SList) -> makeOpType typeof<string list>
            | ThriftAST.BaseField(ThriftAST.BaseType.String) -> makeOpType typeof<string>
    
    let someCaseInfo ftype = 
        let opType = toOpType ftype
        let ctor = opType.GetMethods(BindingFlags.Public ||| BindingFlags.Static) |> Array.find (fun mi -> mi.Name = "Some")
        ctor

    let getOpValue ftype getField:Expr =
        let opType = toOpType ftype
        let value = opType.GetMethods() |> Array.find (fun mi -> mi.Name = "get_Value")
        Expr.Call(getField,value,[])
        
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

    let getWriterMethod (isReq) (f:ThriftAST.FieldType) (oprot:Expr<Thrift.Protocol.TProtocol>) (getField:Expr):Expr = 
        let getExpr =
            match isReq with
            | Some(true) -> getField
            | _ -> getOpValue f getField
        match f with
        | ThriftAST.BaseField(ThriftAST.BaseType.Binary) -> <@@ (%oprot).WriteBinary(%%getExpr:byte []) @@>
        | ThriftAST.BaseField(ThriftAST.BaseType.Bool) -> <@@ (%oprot).WriteBool(%%getExpr:bool) @@>
        | ThriftAST.BaseField(ThriftAST.BaseType.Byte) -> <@@ (%oprot).WriteByte <| sbyte (%%getExpr:byte) @@>
        | ThriftAST.BaseField(ThriftAST.BaseType.Double) -> <@@ (%oprot).WriteDouble(%%getExpr:float) @@>
        | ThriftAST.BaseField(ThriftAST.BaseType.I16) -> <@@ (%oprot).WriteI16(%%getExpr:int16) @@>
        | ThriftAST.BaseField(ThriftAST.BaseType.I32) -> <@@ (%oprot).WriteI32(%%getExpr:int32) @@>
        | ThriftAST.BaseField(ThriftAST.BaseType.I64) -> <@@ (%oprot).WriteI64(%%getExpr:int64) @@>
        | ThriftAST.BaseField(ThriftAST.BaseType.SList) -> <@@ (%oprot).WriteString(%%getExpr:string) @@>
        | ThriftAST.BaseField(ThriftAST.BaseType.String) -> <@@ (%oprot).WriteString(%%getExpr:string) @@>
        // Basicaly from here down I'm faking it
        | ThriftAST.IdentifierField _ -> <@@ fun (field:Thrift.Protocol.TAbstractBase) -> field.Write(%oprot) @@>
        | ThriftAST.ContainerField(ThriftAST.ContainerType.List(_)) -> <@@ fun list -> (%oprot).WriteListBegin(list) @@>
        | ThriftAST.ContainerField(ThriftAST.ContainerType.Map(_)) -> <@@ fun map -> (%oprot).WriteMapBegin(map) @@>
        | ThriftAST.ContainerField(ThriftAST.ContainerType.Set(_)) -> <@@ fun set -> (%oprot).WriteSetBegin(set) @@>
    
    let buildReaderForField ( this:Expr) (iprot:Expr<Thrift.Protocol.TProtocol>) (fieldVar:Expr<Thrift.Protocol.TField>) fieldInfo (ThriftAST.Field(Some(fieldId),isReq,fieldType,_,_)) =
        let reader = getReaderMethod isReq fieldType iprot
        let setField = Expr.PropertySet(this,fieldInfo,reader)
        let targetType = getFieldType fieldType
        let fieldID:Expr<int16> = Expr.Value(int16 fieldId) |> Expr.Cast

        let checkField = <@ (%fieldVar).ID = (%fieldID) && (%fieldVar).Type = %targetType @>
        checkField,setField

    let buildWriterForField (this:Expr) (oprot:Expr<Thrift.Protocol.TProtocol>) (fieldVar:Expr<Thrift.Protocol.TField>) (fieldInfo:ProvidedProperty) (ThriftAST.Field(Some(fieldId),isReq,fieldType,ThriftAST.Identifier(fieldName),_)) =
        let getField = Expr.PropertyGet(this,fieldInfo)
        let fieldId:Expr<int16> = Expr.Value(int16 fieldId) |> Expr.Cast
        let fieldName:Expr<string> = Expr.Value(fieldName) |> Expr.Cast
        let fieldTType:Expr<Thrift.Protocol.TType> = getFieldType fieldType
        let fieldAsObj:Expr<obj> = Expr.Coerce(getField,typeof<obj>) |> Expr.Cast
        let canWrite:Expr<bool> = <@ not (isNull (%fieldAsObj)) @>
        let writer:Expr<unit> = getWriterMethod isReq fieldType oprot getField |> Expr.Cast
        <@
            if (%canWrite) then
                let mutable field = (%fieldVar)
                field.Name <- %fieldName
                field.Type <- %fieldTType
                field.ID <- %fieldId
                (%oprot).WriteFieldBegin(field)
                (%writer)
                (%oprot).WriteFieldEnd()
        @> 

    let createReaderForStruct (fieldNfos:Map<string,ProvidedProperty>) (name,fields) = 
        let buildReader (args:Expr list) =
            let iprot:Expr<Thrift.Protocol.TProtocol> = args.[1] |> Expr.Cast
            let this = args.[0]
            let fieldVar = Var("field",typeof<Thrift.Protocol.TField>) 
            let fieldVarExpr = Expr.Var(fieldVar) |> Expr.Cast
            let buildField = buildReaderForField this iprot (fieldVarExpr)
            let fields = fields 
                         |> normalizeFieldIds 
                         |> snd 
                         |> List.choose (fun f ->
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
                    if field.Type = Thrift.Protocol.TType.Stop then 
                        ignore()
                    else
                        (%applyFunc) field
                        readField (iprot.ReadFieldBegin())
                    
                iprot.ReadStructBegin() |> ignore
                readField (iprot.ReadFieldBegin())
                iprot.ReadStructEnd()
            @>
        buildReader
    let createWriterForStruct (fieldNfos:Map<string,ProvidedProperty>) (name,fields) = 
        let buildWriter (args:Expr list) =
            let oprot:Expr<Thrift.Protocol.TProtocol> = args.[1] |> Expr.Cast
            let this = args.[0]
            let fieldVar = Var("field",typeof<Thrift.Protocol.TField>,true) 
            let fieldVarExpr = Expr.Var(fieldVar) |> Expr.Cast
            let buildField = buildWriterForField this oprot fieldVarExpr
            let fields = fields 
                         |> normalizeFieldIds 
                         |> snd 
                         |> List.choose (fun f ->
                            let (ThriftAST.Field(_,_,_,ThriftAST.Identifier(name),_)) = f
                            match Map.tryFind name fieldNfos with
                            | None -> None
                            | Some fNfo -> Some (buildField fNfo f))
            let allFields = List.foldBack (fun fieldExpr exp -> Expr.Sequential(exp,fieldExpr)) fields <@ () @>.Raw
            let applyWrite apply:Expr<Thrift.Protocol.TField -> unit> = Expr.Lambda(fieldVar,apply) |> Expr.Cast
            <@
                let tstruct = new Thrift.Protocol.TStruct(name)
                (%oprot).WriteStructBegin(tstruct)
                (%(applyWrite allFields)) (new Thrift.Protocol.TField(null,Thrift.Protocol.TType.Stop,0s))
                (%oprot).WriteFieldStop()
                (%oprot).WriteStructEnd()
            @>
        buildWriter 

    let inline createReader fieldInfo s = 
        ProvidedMethod("Read",
                       [ProvidedParameter("tProtocol",typeof<Thrift.Protocol.TProtocol>)],
                       typeof<Void>,
                       InvokeCode = ((createReaderForStruct fieldInfo s) >> Expr.raw))
    
    let inline createWriter fieldInfo s = 
        ProvidedMethod("Write",
                       [ProvidedParameter("tProtocol",typeof<Thrift.Protocol.TProtocol>)],
                       typeof<Void>,
                       InvokeCode = ((createWriterForStruct fieldInfo s) >> Expr.raw))
        

    let buildProperties typeMap (t:ProvidedTypeDefinition) (fields) = 
        let fieldMap = 
            fields 
            |> List.map (PropertyBuilder.createProperty typeMap)
            |> List.fold (fun fieldMap (name,[field;property]) ->
                t.AddMember field
                t.AddMember property
                fieldMap |> Map.add name (property :?> ProvidedProperty)) Map.empty
        fieldMap
    let inline private buildObject typeMap baseType name s = 
        let t = ProvidedTypeDefinition(Naming.toPascal name,Some baseType,IsErased = false)
        let fieldMap = buildProperties typeMap t s
        let ctor = ProvidedConstructor([])
        ctor.InvokeCode <- (fun _ -> <@@ ignore() @@>)
        t.AddMember ctor
        let reader = createReader fieldMap (name,s)
        reader.SetMethodAttrs(MethodAttributes.Virtual)
        let writer = createWriter fieldMap (name,s)
        writer.SetMethodAttrs(MethodAttributes.Virtual)
        t.AddInterfaceImplementation(typeof<Thrift.Protocol.TBase>)
        t.AddInterfaceImplementation(typeof<Thrift.Protocol.TAbstractBase>)
        let tbaseRead = typeof<Thrift.Protocol.TBase>.GetMethods() |> Array.head
        let tbaseWrite = typeof<Thrift.Protocol.TAbstractBase>.GetMethods() |> Array.head
        t.DefineMethodOverride(reader,tbaseRead)
        t.DefineMethodOverride(writer,tbaseWrite)
        t.AddMembers [reader;writer]
        t
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