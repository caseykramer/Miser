module ThriftReader

open FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open ThriftHelpers
open System.Collections.Generic

type ReaderInfo = {
    typeMap:TypeMap
    fieldThriftType:ThriftAST.FieldType
    iprot:Expr<Thrift.Protocol.TProtocol>
    fieldInfo:FieldInfo
}

let getReaderMethod (info:ReaderInfo):Expr = 
    match info.fieldThriftType,info.fieldInfo.optionInfo |> Option.isSome with
    | ThriftAST.BaseField(ThriftAST.BaseType.Binary),false -> <@@ (%info.iprot).ReadBinary() @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.Binary),true -> <@@ Some((%info.iprot).ReadBinary()) @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.Bool),false -> <@@ (%info.iprot).ReadBool() @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.Bool),true -> <@@ Some((%info.iprot).ReadBool()) @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.Byte),false -> <@@ (%info.iprot).ReadByte() @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.Byte),true -> <@@ Some((%info.iprot).ReadByte()) @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.Double),false -> <@@ (%info.iprot).ReadDouble() @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.Double),true -> <@@ Some((%info.iprot).ReadDouble()) @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.I16),false -> <@@ (%info.iprot).ReadI16() @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.I16),true -> <@@ Some((%info.iprot).ReadI16()) @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.I32),false -> <@@ (%info.iprot).ReadI32() @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.I32),true -> <@@ Some((%info.iprot).ReadI32()) @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.I64),false -> <@@ (%info.iprot).ReadI64() @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.I64),true -> <@@ Some((%info.iprot).ReadI64()) @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.SList),false -> <@@ (%info.iprot).ReadString() @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.SList),true -> <@@ Some((%info.iprot).ReadString()) @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.String),false -> <@@ (%info.iprot).ReadString() @@>
    | ThriftAST.BaseField(ThriftAST.BaseType.String),true -> <@@ Some((%info.iprot).ReadString()) @@>
    | ThriftAST.IdentifierField (ThriftAST.Identifier(typeName)),_ -> 
        match info.typeMap.TryGetValue typeName with
        | false,_ -> failwithf "Unknown type '%s' in field" typeName
        | true,fieldType ->
            let getValue = 
                if fieldType.IsEnum then
                    Expr.Coerce(<@ (%info.iprot).ReadI32() @>,fieldType)
                else 
                    let ctor = fieldType.GetConstructors() |> Array.head
                    let newVal:Expr<Thrift.Protocol.TBase> = Expr.Coerce(Expr.NewObject(ctor,[]),typeof<Thrift.Protocol.TBase>) |> Expr.Cast
                    <@@ let field = (%newVal)
                        field.Read(%info.iprot) @@>
            getValue
    // TODO: Finish decoding
    | ThriftAST.ContainerField(ThriftAST.ContainerType.List(_)),_ -> <@@ (%info.iprot).ReadListBegin() @@>
    | ThriftAST.ContainerField(ThriftAST.ContainerType.Map(_)),_ -> <@@ (%info.iprot).ReadMapBegin() @@>
    | ThriftAST.ContainerField(ThriftAST.ContainerType.Set(_)),_ -> <@@ (%info.iprot).ReadSetBegin() @@>
            

let buildReaderForField (this:Expr) (iprot:Expr<Thrift.Protocol.TProtocol>) (fieldVar:Expr<Thrift.Protocol.TField>) typeMap (fieldInfo:FieldInfo) (ThriftAST.Field(Some(fieldId),_,fieldType,_,_)) =
    let readerInfo = {
        typeMap = typeMap
        fieldThriftType = fieldType
        iprot = iprot
        fieldInfo = fieldInfo
    }
    let reader = getReaderMethod readerInfo
    let setField = Expr.FieldSet(this,fieldInfo.field,reader)
    let targetType = getFieldTTypeExpr fieldType
    let fieldID:Expr<int16> = Expr.Value(int16 fieldId) |> Expr.Cast

    let checkField = <@ (%fieldVar).ID = (%fieldID) && (%fieldVar).Type = %targetType @>
    checkField,setField

let createReaderForStruct typeMap (fieldNfos:FieldMap) (_,fields) = 
    let buildReader (args:Expr list) =
        let iprot:Expr<Thrift.Protocol.TProtocol> = args.[1] |> Expr.Cast
        let this = args.[0]
        let fieldVar = Var("field",typeof<Thrift.Protocol.TField>) 
        let fieldVarExpr = Expr.Var(fieldVar) |> Expr.Cast
        let buildField = buildReaderForField this iprot (fieldVarExpr) typeMap
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
        <@ let iprot:Thrift.Protocol.TProtocol = (%%(args.[1]):Thrift.Protocol.TProtocol)
           let rec readField (field:Thrift.Protocol.TField) = 
               if field.Type = Thrift.Protocol.TType.Stop then 
                   ignore()
               else
                   (%applyFunc) field
                   readField (iprot.ReadFieldBegin())
               
           iprot.ReadStructBegin() |> ignore
           readField (iprot.ReadFieldBegin())
           iprot.ReadStructEnd() @>
    buildReader
