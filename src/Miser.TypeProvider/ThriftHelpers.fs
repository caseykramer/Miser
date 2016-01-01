module ThriftHelpers

open ThriftAST
open System.Collections.Generic
open ProviderImplementation.ProvidedTypes
open System
open FSharp.Quotations

type TypeMap = Dictionary<string,ProvidedTypeDefinition>

type ObjectType = 
    | BaseType of Type
    | ThriftType of ProvidedTypeDefinition
    | ContainerType of Type*Type list
    | Optional of ObjectType with
        member x.Type
            with get() = 
                match x with
                | BaseType t                    
                | ContainerType (t,_) -> t
                | ThriftType t -> upcast t
                | Optional(t) -> t.Type
        member x.IsOptionalThriftType
            with get() = 
                match x with
                | Optional(ThriftType _) -> true
                | _ -> false
        static member Type_ = fun (x:ObjectType) -> x.Type

type GenericFieldInfo = {
    finalType:Type
    argType:Type list
}

type OptionalFieldInfo = {
    optionType:Type
    valueType:Type } with
        member x.GetValue opExpr:Expr = 
            let valueProp = x.optionType.GetMethod("get_Value")
            Expr.Call(opExpr,valueProp,[])
        member x.HasValue opExpr:Expr<bool> = 
            let hasValueProp = x.optionType.GetMethod("get_IsSome")
            Expr.Call(hasValueProp,[opExpr]) |> Expr.Cast
        member x.MakeSome opExpr:Expr = 
            let someMethod = x.optionType.GetMethod("Some") // Static method returing Some x
            Expr.Call(someMethod,[opExpr])

type FieldInfo = {
    originalName:string
    field:ProvidedField
    fieldType:Type
    fieldTType:Thrift.Protocol.TType
    genericInfo:GenericFieldInfo option
    optionInfo:OptionalFieldInfo option }

type FieldMap = Map<string,FieldInfo>

let baseOption = typeof<Option<_>>.GetGenericTypeDefinition()
let baseList = typeof<List<_>>.GetGenericTypeDefinition()
let baseDict = typeof<System.Collections.Generic.Dictionary<_,_>>.GetGenericTypeDefinition()
let baseSet = typeof<System.Collections.Generic.HashSet<_>>.GetGenericTypeDefinition()
let baseEnumerable = typeof<System.Collections.Generic.IEnumerable<_>>.GetGenericTypeDefinition()

let makeOption (t:Type) = 
    match t with
    | :? ProvidedTypeDefinition -> ProvidedTypeBuilder.MakeGenericType(baseOption,[t])
    | _ -> baseOption.MakeGenericType([|t|])
let makeList (t:Type) = 
    match t with
    | :? ProvidedTypeDefinition -> ProvidedTypeBuilder.MakeGenericType(baseList,[t])
    | _ -> baseList.MakeGenericType([|t|])
let makeSet (t:Type) = 
    match t with
    | :? ProvidedTypeDefinition -> ProvidedTypeBuilder.MakeGenericType(baseSet,[t])
    | _ -> baseSet.MakeGenericType([|t|])
let makeMap (k:Type) (v:Type) = 
    match k,v with 
    | :? ProvidedTypeDefinition, _
    | _,:? ProvidedTypeDefinition -> ProvidedTypeBuilder.MakeGenericType(baseDict,[k;v])
    | _ -> baseDict.MakeGenericType([|k;v|])
let makeEnumerable (t:Type) = 
    match t with
    | :? ProvidedTypeDefinition -> ProvidedTypeBuilder.MakeGenericType(baseEnumerable,[t])
    | _ -> baseEnumerable.MakeGenericType([|t|])

let isOption (t:Type) = t.IsGenericType && t.GetGenericTypeDefinition() = baseOption 
    
let normalizeFieldIds (fields:ThriftAST.FieldDefinition list) = 
    fields 
    |> List.fold (fun (counter,fields) field -> 
        match field with
        | ThriftAST.Field(Some id,_,_,_,_) as f -> (id+1L,f::fields)
        | ThriftAST.Field(None,isReq,fieldType,ident,value) -> (counter + 1L,ThriftAST.Field(Some counter,isReq,fieldType,ident,value)::fields) )(1L,[])
 

let getFieldTType (f:ThriftAST.FieldType) = 
    match f with
    | ThriftAST.BaseField(ThriftAST.BaseType.Binary) -> Thrift.Protocol.TType.Byte
    | ThriftAST.BaseField(ThriftAST.BaseType.Bool) -> Thrift.Protocol.TType.Bool
    | ThriftAST.BaseField(ThriftAST.BaseType.Byte) -> Thrift.Protocol.TType.Byte
    | ThriftAST.BaseField(ThriftAST.BaseType.Double) -> Thrift.Protocol.TType.Double
    | ThriftAST.BaseField(ThriftAST.BaseType.I16) -> Thrift.Protocol.TType.I16
    | ThriftAST.BaseField(ThriftAST.BaseType.I32) -> Thrift.Protocol.TType.I32
    | ThriftAST.BaseField(ThriftAST.BaseType.I64) -> Thrift.Protocol.TType.I64
    | ThriftAST.BaseField(ThriftAST.BaseType.SList) -> Thrift.Protocol.TType.List
    | ThriftAST.BaseField(ThriftAST.BaseType.String) -> Thrift.Protocol.TType.String
    | ThriftAST.IdentifierField _ -> Thrift.Protocol.TType.Struct
    | ThriftAST.ContainerField(ThriftAST.ContainerType.List(_)) -> Thrift.Protocol.TType.List
    | ThriftAST.ContainerField(ThriftAST.ContainerType.Map(_)) -> Thrift.Protocol.TType.Map
    | ThriftAST.ContainerField(ThriftAST.ContainerType.Set(_)) -> Thrift.Protocol.TType.Set

let getFieldTTypeExpr (f:ThriftAST.FieldType):Expr<Thrift.Protocol.TType> = 
    let ttype = getFieldTType f
    Expr.Value(ttype) |> Expr.Cast

let (|AsOption|) (t:Type) = AsOption (makeOption t)

let getType (typeMap:Dictionary<string,ProvidedTypeDefinition>) (ftype:ThriftAST.FieldType) (isReq:bool option):ObjectType = 
    let rec buildType (t:ThriftAST.FieldType) =
        match t with
        | ThriftAST.BaseField(baseType) ->
            match baseType with
            | ThriftAST.BaseType.Binary -> BaseType(typeof<byte array>)
            | ThriftAST.BaseType.Bool   -> BaseType(typeof<bool>)
            | ThriftAST.BaseType.Byte   -> BaseType(typeof<byte>)
            | ThriftAST.BaseType.Double -> BaseType(typeof<double>)
            | ThriftAST.BaseType.I16    -> BaseType(typeof<int16>)
            | ThriftAST.BaseType.I32    -> BaseType(typeof<int>)
            | ThriftAST.BaseType.I64    -> BaseType(typeof<int64>)
            | ThriftAST.BaseType.SList  -> BaseType(typeof<string array>)
            | ThriftAST.BaseType.String -> BaseType(typeof<string>)
        | ThriftAST.ContainerField(ThriftAST.ContainerType.List(t)) ->
            let baseType = 
                buildType t 
                    |> ObjectType.Type_ 
            let listType = makeList baseType
            ContainerType (listType,[baseType])
        | ThriftAST.ContainerField(ThriftAST.ContainerType.Set(t)) ->
            let baseType = 
                buildType t 
                |> ObjectType.Type_
            let setType = makeSet baseType
            ContainerType (setType,[baseType])
        | ThriftAST.ContainerField(ThriftAST.ContainerType.Map(k,v)) ->
            let key = buildType k |> ObjectType.Type_
            let value = buildType v |> ObjectType.Type_
            let mapType = makeMap key value 
            ContainerType (mapType,[key;value])
        | ThriftAST.IdentifierField(ThriftAST.Identifier(ident)) ->
            match typeMap.TryGetValue(ident) with
            | false,_ -> failwithf "Type: '%s' is not defined" ident
            | true,t -> ThriftType t
    match isReq with
    | Some v -> if v then buildType ftype else Optional (buildType ftype)
    | _ -> Optional (buildType ftype)
