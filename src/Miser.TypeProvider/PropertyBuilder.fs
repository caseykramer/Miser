module internal PropertyBuilder

open Util
open System
open System.Reflection
open ProviderImplementation.ProvidedTypes
open ThriftHelpers

let createField typeMap (field:ThriftAST.FieldDefinition) = 
    match field with
    | ThriftAST.Field(_,isReq,fieldType,ThriftAST.Identifier(fName),_) ->
        let resolvedFieldType = getType typeMap fieldType isReq 
        let fieldData t providedField = 
            { originalName = fName
              field = providedField
              fieldType = t
              fieldTType = getFieldTType fieldType
              genericInfo = None
              optionInfo = None
            }
        match resolvedFieldType with
        | Optional (BaseType (t)) ->
            let optionT = makeOption t
            let f = ProvidedField(Naming.toPascal fName,optionT)
            f.SetFieldAttributes FieldAttributes.Public
            { fieldData t f with optionInfo = Some {optionType = optionT; valueType = t; }}
        | BaseType t -> 
            let f = ProvidedField(Naming.toPascal fName,t)
            f.SetFieldAttributes FieldAttributes.Public
            fieldData t f
        | Optional (ThriftType (t)) -> 
            let optionT = makeOption t
            let f = ProvidedField(Naming.toPascal fName,optionT)
            f.SetFieldAttributes FieldAttributes.Public
            { fieldData t f with optionInfo = Some {optionType = optionT; valueType = t}}
        | ThriftType t -> 
            let f = ProvidedField(Naming.toPascal fName,t)
            f.SetFieldAttributes FieldAttributes.Public
            fieldData t f
        | Optional (ContainerType (t,args)) ->
            let optionT = makeOption t
            let f = ProvidedField(Naming.toPascal fName,optionT)
            f.SetFieldAttributes FieldAttributes.Public
            { fieldData t f with 
                optionInfo = Some { optionType = optionT; valueType = t; }
                genericInfo = Some { finalType = t; argType = args }}
        | ContainerType (t,args) -> 
            let f = ProvidedField(Naming.toPascal fName,t)
            f.SetFieldAttributes FieldAttributes.Public
            { fieldData t f with genericInfo = Some { finalType = t; argType = args }} 
        | t -> 
            let f = ProvidedField(Naming.toPascal fName,t.Type)
            f.SetFieldAttributes FieldAttributes.Public
            fieldData t.Type f  