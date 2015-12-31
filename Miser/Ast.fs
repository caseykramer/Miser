module Ast

open System.Diagnostics

type Comment = 
     | Comment of string
     | DocComment of string
     | CommentBlock of string

[<StructuredFormatDisplay("Identifier({Display})");DebuggerDisplay("Identifier({Display})")>]
type Identifier = Identifier of string with
    member this.Display
            with get() =
                match this with
                | Identifier s -> s
    override this.ToString() = 
            sprintf "%A" this
[<StructuredFormatDisplay("{Display}");DebuggerDisplay("{Display}")>]
type Literal = 
     | StringLiteral of string
     | NumericLiteral of int64
     | BoolLiteral of bool
     | DecimalLiteral of float with
        member this.Display
            with get() =
                match this with
                | BoolLiteral b -> if b then "true" else "false"
                | StringLiteral s -> sprintf "\"%s\"" s
                | NumericLiteral i -> sprintf "%i" i
                | DecimalLiteral d -> sprintf "%f" d
        override this.ToString() = 
            sprintf "%A" this
[<StructuredFormatDisplay("Constant:{Display}");DebuggerDisplay("Constant:{Display}")>]
type ConstantValue =
     | LiteralConstant of Literal
     | IdentConstant of Identifier
     | IntConstant of int
     | DoubleConstant of float
     | ListConstant of ConstantValue list
     | MapConstant of Map<ConstantValue,ConstantValue> with
        member this.Display
            with get() = 
                match this with
                | LiteralConstant (literal) -> sprintf "%A" literal
                | IdentConstant (identifier) -> sprintf "%A" identifier
                | IntConstant (intVal) -> sprintf "%i" intVal
                | DoubleConstant (doubleVal) -> sprintf "%f" doubleVal
                | ListConstant (values) -> sprintf "%A" values
                | MapConstant (values) -> sprintf "%A" values
        override this.ToString() = 
            sprintf "%A" this
[<StructuredFormatDisplay("{Display}");DebuggerDisplay("{Display}")>]
type BaseType = 
     | Bool
     | Byte
     | I16
     | I32
     | I64
     | Double
     | String
     | Binary
     | SList  with // slist seems to be deprecated see: https://issues.apache.org/jira/browse/THRIFT-1994
        member this.Display
            with get() = 
                match this with
                | Bool -> "Bool"
                | Byte -> "Byte"
                | I16 -> "I16"
                | I32 -> "I32"
                | I64 -> "I64"
                | Double -> "Double"
                | String -> "String"
                | Binary -> "Binary"
                | SList -> "SList"
        override this.ToString() = 
            sprintf "%A" this
[<StructuredFormatDisplay("Container: {Display}");DebuggerDisplay("Container: {Display}")>]
type ContainerType = 
     | Map of FieldType*FieldType
     | List of FieldType
     | Set of FieldType with
        member this.Display
            with get() = 
                match this with
                | Map (key,value) -> sprintf "Map(%A,%A)" key value
                | List (listType) -> sprintf "List(%A)" listType
                | Set (setType)   -> sprintf "Set(%A)" setType
        override this.ToString() = 
            sprintf "%A" this
and [<StructuredFormatDisplay("FieldType: {Display}");DebuggerDisplay("FieldType: {Display}")>]
FieldType = 
     | IdentifierField of Identifier
     | BaseField of BaseType
     | ContainerField of ContainerType with
        member this.Display
            with get() = 
                match this with
                | IdentifierField (ident) -> sprintf "%A" ident
                | BaseField (field) -> sprintf "%A" field
                | ContainerField (field) -> sprintf "%A" field
        override this.ToString() = 
            sprintf "%A" this
[<StructuredFormatDisplay("DefinitionType: {Display}");DebuggerDisplay("DefinitionType: {Display}")>]
type DefinitionType = 
     | BaseDefinition of BaseType
     | ContainerDefinition of ContainerType with
        member this.Display
            with get() =
                match this with
                | BaseDefinition (baseType) -> sprintf "%A" baseType
                | ContainerDefinition (containerType) -> sprintf "%A" containerType
        override this.ToString() = 
            sprintf "%A" this
[<StructuredFormatDisplay("Field: {Display}");DebuggerDisplay("Field: {Display}")>]
type Field =
     | NumberedField of fieldNum:int*field:Field
     | RequiredField of field:Field
     | OptionalField of field:Field
     | Field of fieldType:FieldType*name:Identifier*defaultValue:ConstantValue option with
        member this.Display
            with get() = 
                match this with
                | NumberedField (fieldNum,field) -> sprintf "[%i;%A]" fieldNum field
                | RequiredField (field) -> sprintf "(Required: %A)" field
                | OptionalField (field) -> sprintf "(Optional: %A)" field
                | Field (ftype,name,defaultValue) -> sprintf "{type: %A, name: %s, default: %A}" ftype name.Display defaultValue
        override this.ToString() = 
            sprintf "%A" this

[<StructuredFormatDisplay("Exception: {Display}");DebuggerDisplay("Exception: {Display}")>]
type Exception = 
     | Exception of name:Identifier*fields:Field list with
        member this.Display
            with get() = 
                match this with
                | Exception (name,fields) -> sprintf "{name:%s,fields:%A}" name.Display fields
        override this.ToString() = 
            sprintf "%A" this
[<StructuredFormatDisplay("{Display}");DebuggerDisplay("{Display}")>]
type Function = 
     | OnewayFunction of Function
     | VoidFunction of name:Identifier * parameters:Field list * throws:Field list
     | Function of returnType:FieldType * name:Identifier * parameters:Field list * throws:Field list with
        member this.Display
            with get() = 
                match this with
                | OnewayFunction (func) -> sprintf "(oneway %A)" func
                | VoidFunction (name,paramList,throws) -> sprintf "(void %s (%s) throws: %A" name.Display (paramList |> List.fold (fun l (item:Field) -> sprintf "%s,%A" l item) "" ) throws
                | Function (returnType,name,paramList,throws) -> sprintf "(%A %s (%s) throws: %A" returnType name.Display (paramList |> List.fold (fun l item -> sprintf "%s,%A" l item) "") throws
        override this.ToString() = 
            sprintf "%A" this
[<StructuredFormatDisplay("Service: {Display}");DebuggerDisplay("Service: {Display}")>]
type Service = 
     | Service of serviceName:Identifier * functions:Function list * parent:Identifier option with
        member this.Display
            with get() = 
                match this with
                | Service (name,functions,parent) -> 
                    match parent with
                    | None -> sprintf "%s {%A}" name.Display functions
                    | Some parent -> sprintf "%s extends %s {%A}" name.Display parent.Display functions
        override this.ToString() = 
            sprintf "%A" this
[<StructuredFormatDisplay("Struct: {Display}");DebuggerDisplay("Struct: {Display}")>]
type Struct = 
     | Struct of structName:Identifier * fields:Field list with
        member this.Display
            with get() = 
                match this with
                | Struct (name,fields) -> sprintf "%s {%A}" name.Display fields
        override this.ToString() = 
            sprintf "%A" this
[<StructuredFormatDisplay("Union: {Display}");DebuggerDisplay("Union: {Display}")>]
type Union = 
     | Union of unionName:Identifier * fields:Field list with
        member this.Display
            with get() = 
                match this with 
                | Union (name,fields) -> sprintf "%s {%A}" name.Display fields
        override this.ToString() = 
            sprintf "%A" this
type StringEnum = 
     | SEnum of senumName:Identifier*values:Literal list

[<StructuredFormatDisplay("Enum: {Display}");DebuggerDisplay("Enum: {Display}")>]
type Enum = 
     | Enum of enumName:Identifier*values:(int*Identifier) list with
        member this.Display
            with get() = 
                match this with
                | Enum (name,values) -> sprintf "%s { %s }" name.Display (values |> List.fold (fun l item -> sprintf "%s,%s = %i" l (snd item).Display (fst item)) "")
        override this.ToString() = 
            sprintf "%A" this
[<StructuredFormatDisplay("TypeDef: {Display}");DebuggerDisplay("TypeDef: {Display}")>]
type TypeDef =
     | TypeDef of DefinitionType*Identifier with 
        member this.Display
            with get() = 
                match this with
                | TypeDef (defType,name) -> sprintf "%A %s" defType name.Display
        override this.ToString() = 
            sprintf "%A" this
[<StructuredFormatDisplay("Const: {Display}");DebuggerDisplay("Const: {Display}")>]
type Const = 
     | Const of constType:FieldType*constName:Identifier*constValue:ConstantValue with
        member this.Display
            with get() = 
                match this with
                | Const (constType,name,value) -> sprintf "%s:%A = %A" name.Display constType value
        override this.ToString() = 
            sprintf "%A" this

type TType = 
    | Const of Const
    | TypeDef of TypeDef
    | DefinitionType of DefinitionType
    | Enum of Enum
    | Union of Union
    | Struct of Struct
    | Exception of Exception
    | Service of Service
    | Function of Function
[<StructuredFormatDisplay("Definition: ({Display})");DebuggerDisplay("Definition: ({Display})")>]
type Definition = 
     | ConstDefinition of Const
     | TypeDefinition of TypeDef
     | EnumDefinition of Enum
     | SEnumDefinition of StringEnum
     | StructDefinition of Struct
     | ExceptionDefinition of Exception
     | ServiceDefinition of Service
     | UnionDefinition of Union with 
        member this.Display
            with get() =
                match this with
                | ConstDefinition item -> sprintf "%A" item
                | TypeDefinition item -> sprintf "%A" item
                | EnumDefinition item -> sprintf "%A" item
                | SEnumDefinition item -> sprintf "%A" item
                | StructDefinition item -> sprintf "%A" item
                | ExceptionDefinition item -> sprintf "%A" item
                | ServiceDefinition item -> sprintf "%A" item
                | UnionDefinition item -> sprintf "%A" item
        override this.ToString() = 
            sprintf "%A" this
[<StructuredFormatDisplay("{Display}");DebuggerDisplay("{Display}")>]
type NamespaceScope = 
     | Any
     | Cpp
     | Java
     | Py
     | Perl
     | Rb
     | Cocoa
     | CSharp 
     | C_Glib 
     | Py_Twisted 
     | Go
     | Delphi
     | Javascript
     | Smalltalk
     | Php
     | Other of string with
        member this.Display
            with get() = 
                match this with
                | Any -> "Any"
                | Cpp -> "Cpp"
                | Java -> "Java"
                | Py -> "Python"
                | Perl -> "Perl"
                | Rb -> "Ruby"
                | Cocoa -> "Cocoa"
                | CSharp -> "C#"
                | C_Glib -> "C Glib"
                | Py_Twisted -> "Py.Twisted"
                | Go -> "Go"
                | Delphi -> "Delphi"
                | Javascript -> "Javascript"
                | Smalltalk -> "Smalltalk"
                | Php -> "Php"
                | Other (name) -> name
        override this.ToString() = 
            sprintf "%A" this

[<StructuredFormatDisplay("Namespace: {Display}");DebuggerDisplay("Namespace: {Display}")>]
type Namespace = 
     Namespace of NamespaceScope * Identifier with
        member this.Display 
            with get() =  
                match this with
                | Namespace (scope,identifier) -> sprintf "%A %A" scope identifier
        override this.ToString() = 
            sprintf "%A" this
[<StructuredFormatDisplay("Include: {Display}");DebuggerDisplay("Include: {Display}")>]
type Include = 
     | Include of Literal with
        member this.Display
            with get() = 
                match this with
                | Include (literal) -> sprintf "\"%s\"" literal.Display
        override this.ToString() = 
            sprintf "%A" this

[<StructuredFormatDisplay("Header: {Display}");DebuggerDisplay("Header: {Display}")>]
type Header =
     | IncludeHeader of Include
     | NamespaceHeader of Namespace with
        member this.Display
            with get() = 
                match this with
                | IncludeHeader i -> sprintf "%A" i
                | NamespaceHeader n -> sprintf "%A" n
        override this.ToString() = 
            sprintf "%A" this
type Document = { Headers:Header list; Definitions:Definition list }
type Program = Program of Document list