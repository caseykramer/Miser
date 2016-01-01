module ThriftWriter

open System
open ThriftHelpers
open Thrift.Protocol
open FSharp.Quotations
open ProviderImplementation.ProvidedTypes

type WriterData = {
    thriftFieldType:ThriftAST.FieldType
    typeMap:TypeMap
    oprot:Expr<TProtocol>
    fieldInfo:FieldInfo
    getField:Expr
}

let rec getWriterExpr (info:WriterData):Expr<unit> = 
    let writeEnumerable lType t (tWriteBegin:Expr<Thrift.Protocol.TType -> int -> unit>) = 
        let listTType = getFieldTTypeExpr lType
        let elementType = match info.fieldInfo.genericInfo with
                          | None -> failwith "Expected generic type information"
                          | Some t -> t.argType |> List.head        
        let getList = match info.fieldInfo.optionInfo with
                      | None -> info.getField
                      | Some i -> i.GetValue info.getField
        let enumerableType = typeof<System.Collections.IEnumerable>
        let asEnumerable:Expr<System.Collections.IEnumerable> = Expr.Coerce(getList,enumerableType) |> Expr.Cast
        let getCount:Expr<int> = 
            let countProp = info.fieldInfo.fieldType.GetMethod("get_Count")
            Expr.Call(getList,countProp,[]) |> Expr.Cast
        let itemVar = Var("item",typeof<obj>)
        let getWriter:Expr<obj -> unit> = 
            let castCurrent = Expr.Coerce(Expr.Var(itemVar),elementType)
            Expr.Lambda(itemVar,getWriterExpr { info with thriftFieldType = lType; getField = castCurrent; fieldInfo = { info.fieldInfo with optionInfo = None }}) |> Expr.Cast
        <@ let count:int = (%getCount)
           let ttype = %listTType
           (%tWriteBegin) ttype count
           let enumerator = (%asEnumerable).GetEnumerator()
           while enumerator.MoveNext() do
               (%getWriter) enumerator.Current @>
    
    match info.thriftFieldType,info.fieldInfo.optionInfo.IsNone with
    | ThriftAST.BaseField(ThriftAST.BaseType.Binary),true -> <@ (%info.oprot).WriteBinary(%%info.getField:byte []) @>
    | ThriftAST.BaseField(ThriftAST.BaseType.Binary),false -> <@ match (%%info.getField:Option<byte[]>) with 
                                                                 | None -> ignore() 
                                                                 | Some v -> (%info.oprot).WriteBinary(v) @>
    | ThriftAST.BaseField(ThriftAST.BaseType.Bool),true -> <@ (%info.oprot).WriteBool(%%info.getField:bool) @>
    | ThriftAST.BaseField(ThriftAST.BaseType.Bool),false -> <@ match (%%info.getField:Option<bool>) with
                                                               | None -> ignore()
                                                               | Some v -> (%info.oprot).WriteBool(v) @>
    | ThriftAST.BaseField(ThriftAST.BaseType.Byte),true -> <@ (%info.oprot).WriteByte <| sbyte (%%info.getField:byte) @>
    | ThriftAST.BaseField(ThriftAST.BaseType.Byte),false -> <@ match (%%info.getField:Option<byte>) with
                                                               | None -> ignore()
                                                               | Some v -> (%info.oprot).WriteByte(sbyte v) @>
    | ThriftAST.BaseField(ThriftAST.BaseType.Double),true -> <@ (%info.oprot).WriteDouble(%%info.getField:float) @>
    | ThriftAST.BaseField(ThriftAST.BaseType.Double),false -> <@ match (%%info.getField:Option<float>) with
                                                                 | None -> ignore()
                                                                 | Some v -> (%info.oprot).WriteDouble(v) @>
    | ThriftAST.BaseField(ThriftAST.BaseType.I16),true -> <@ (%info.oprot).WriteI16(%%info.getField:int16) @>
    | ThriftAST.BaseField(ThriftAST.BaseType.I16),false -> <@ match (%%info.getField:Option<int16>) with
                                                              | None -> ignore()
                                                              | Some v -> (%info.oprot).WriteI16(v) @> 
    | ThriftAST.BaseField(ThriftAST.BaseType.I32),true -> <@ (%info.oprot).WriteI32(%%info.getField:int32) @>
    | ThriftAST.BaseField(ThriftAST.BaseType.I32),false -> <@ match (%%info.getField:Option<int32>) with
                                                              | None -> ignore()
                                                              | Some v -> (%info.oprot).WriteI32(v) @>
    | ThriftAST.BaseField(ThriftAST.BaseType.I64),true -> <@ (%info.oprot).WriteI64(%%info.getField:int64) @>
    | ThriftAST.BaseField(ThriftAST.BaseType.I64),false -> <@ match (%%info.getField:Option<int64>) with
                                                              | None -> ignore()
                                                              | Some v -> (%info.oprot).WriteI64(v) @>
    | ThriftAST.BaseField(ThriftAST.BaseType.SList),true -> <@ (%info.oprot).WriteString(%%info.getField:string) @>
    | ThriftAST.BaseField(ThriftAST.BaseType.SList),false -> <@ match (%%info.getField:Option<string>) with
                                                                | None -> ignore()
                                                                | Some v -> (%info.oprot).WriteString(v) @>
    | ThriftAST.BaseField(ThriftAST.BaseType.String),true -> <@ (%info.oprot).WriteString(%%info.getField:string) @>
    | ThriftAST.BaseField(ThriftAST.BaseType.String),false -> <@ match (%%info.getField:Option<string>) with
                                                                 | None -> ignore()
                                                                 | Some v -> (%info.oprot).WriteString(v) @>
    | ThriftAST.IdentifierField (ThriftAST.Identifier(name)),isReq -> 
        match info.typeMap.TryGetValue(name) with
        | false,_ -> failwithf "Invalid field type: '%s'"name
        | true,(t:ProvidedTypeDefinition) ->
            if t.IsEnum then
                let getValue:Expr<int> = 
                    match info.fieldInfo.optionInfo with
                    | None -> Expr.Coerce(info.getField,typeof<int>) |> Expr.Cast
                    | Some v -> Expr.Coerce(v.GetValue info.getField,typeof<int>) |> Expr.Cast                    
                <@ (%info.oprot).WriteI32(%getValue) @>
            else
                let getExpr:Expr<Thrift.Protocol.TAbstractBase> = 
                    match info.fieldInfo.optionInfo with
                    | Some v ->
                        let getValue = v.GetValue info.getField
                        Expr.Coerce(getValue,typeof<Thrift.Protocol.TAbstractBase>) |> Expr.Cast
                    | None -> Expr.Coerce(info.getField,typeof<Thrift.Protocol.TAbstractBase>) |> Expr.Cast
                <@ (%getExpr).Write(%info.oprot) @>
    
    | ThriftAST.ContainerField(ThriftAST.ContainerType.List(lType)) as t,_ -> 
        writeEnumerable lType t <@ fun ttype count -> (%info.oprot).WriteListBegin(Thrift.Protocol.TList(ttype,count)) @>
    | ThriftAST.ContainerField(ThriftAST.ContainerType.Set(sType)) as t,_ ->
        writeEnumerable sType t <@ fun ttype count -> (%info.oprot).WriteSetBegin(Thrift.Protocol.TSet(ttype,count)) @>
    // Basicaly from here down I'm faking it    
    | ThriftAST.ContainerField(ThriftAST.ContainerType.Map(_)),_ -> <@ (%info.oprot).WriteMapBegin(%%info.getField) @>

let buildWriterForField (this:Expr) (oprot:Expr<Thrift.Protocol.TProtocol>) (fieldVar:Expr<Thrift.Protocol.TField>) (typeMap:TypeMap) (fieldInfo:FieldInfo) (ThriftAST.Field(Some(fieldId),isReq,fieldType,ThriftAST.Identifier(fieldName),_)) =
    let getField = Expr.FieldGet(this,fieldInfo.field)
    let fieldId:Expr<int16> = Expr.Value(int16 fieldId) |> Expr.Cast
    let fieldName:Expr<string> = Expr.Value(fieldName) |> Expr.Cast
    let fieldTType = 
        let defaultTType = getFieldTTypeExpr fieldType
        match fieldType with
        | ThriftAST.IdentifierField(ThriftAST.Identifier(typeName)) ->
            match typeMap.TryGetValue typeName with
            | true, t when t.IsEnum -> Expr.Value(Thrift.Protocol.TType.I32) |> Expr.Cast
            | _ -> defaultTType
        | _ -> defaultTType
    let canWrite:Expr<bool> = 
        match fieldInfo.optionInfo with
        | None -> 
            let fieldAsObj = Expr.Coerce(getField,typeof<obj>)
            <@ (%%fieldAsObj) <> null @>
        | Some opInfo -> opInfo.HasValue getField
            
    let writer:Expr<unit> = getWriterExpr { thriftFieldType = fieldType; oprot = oprot; typeMap= typeMap; getField = getField; fieldInfo = fieldInfo } |> Expr.Cast
    <@ if (%canWrite) then
           let mutable field = (%fieldVar)
           field.Name <- %fieldName
           field.Type <- %fieldTType
           field.ID <- %fieldId
           (%oprot).WriteFieldBegin(field)
           (%writer)
           (%oprot).WriteFieldEnd() @> 

let createWriterForStruct typeMap (fieldNfos:FieldMap) (name,fields) = 
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
                        | Some fNfo -> Some (buildField typeMap fNfo f))
        let allFields = List.foldBack (fun fieldExpr exp -> Expr.Sequential(exp,fieldExpr)) fields <@ () @>.Raw
        let applyWrite apply:Expr<Thrift.Protocol.TField -> unit> = Expr.Lambda(fieldVar,apply) |> Expr.Cast
        <@ let tstruct = new Thrift.Protocol.TStruct(name)
           (%oprot).WriteStructBegin(tstruct)
           (%(applyWrite allFields)) (new Thrift.Protocol.TField(null,Thrift.Protocol.TType.Stop,0s))
           (%oprot).WriteFieldStop()
           (%oprot).WriteStructEnd() @>
    buildWriter