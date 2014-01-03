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

[<StructuredFormatDisplay("{Display}");DebuggerDisplay("{Display}")>]
type Literal = 
     | StringLiteral of string
     | NumericLiteral of int64
     | DecimalLiteral of float with
        member this.Display
            with get() =
                match this with
                | StringLiteral s -> sprintf "\"%s\"" s
                | NumericLiteral i -> sprintf "%i" i
                | DecimalLiteral d -> sprintf "%f" d

type ConstantValue =
     | LiteralConstant of Literal
     | IdentConstant of Identifier
     | IntConstant of int
     | DoubleConstant of float
     | ListConstant of ConstantValue list
     | MapConstant of Map<ConstantValue,ConstantValue>

type BaseType = 
     | Bool
     | Byte
     | I16
     | I32
     | I64
     | Double
     | String
     | Binary
     | SList // slist seems to be deprecated see: https://issues.apache.org/jira/browse/THRIFT-1994

type ContainerType = 
     | Map of FieldType*FieldType
     | List of FieldType
     | Set of FieldType
and FieldType = 
     | IdentifierField of Identifier
     | BaseField of BaseType
     | ContainerField of ContainerType

type DefinitionType = 
     | BaseDefinition of BaseType
     | ContainerDefinition of ContainerType

type Field =
     | NumberedField of int*Field
     | RequiredField of Field
     | OptionalField of Field
     | Field of FieldType*Identifier*ConstantValue option

type Exception = 
     | Exception of Identifier*Field list

type Function = 
     | OnewayFunction of Function
     | VoidFunction of name:Identifier * parameters:Field list * throws:Field list
     | Function of returnType:FieldType * name:Identifier * parameters:Field list * throws:Field list

type Service = 
     | Service of Identifier * Function list * Identifier option

type Struct = 
     | Struct of Identifier * Field list

type Union = 
     | Union of Identifier * Field list

type StringEnum = 
     | SEnum of Identifier*Literal list

type Enum = 
     | Enum of Identifier*(int*Identifier) list

type TypeDef =
     | TypeDef of DefinitionType*Identifier

type Const = 
     | Const of FieldType*Identifier*ConstantValue

type Definition = 
     | ConstDefinition of Const
     | TypeDefinition of TypeDef
     | EnumDefinition of Enum
     | SEnumDefinition of StringEnum
     | StructDefinition of Struct
     | ExceptionDefinition of Exception
     | ServiceDefinition of Service
     | UnionDefinition of Union

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

[<StructuredFormatDisplay("Namespace: {Display}");DebuggerDisplay("Namespace: {Display}")>]
type Namespace = 
     Namespace of NamespaceScope * Identifier with
        member this.Display 
            with get() =  
                match this with
                | Namespace (scope,identifier) -> sprintf "%A %A" scope identifier

type Include = 
     | Include of Literal

[<StructuredFormatDisplay("Header: {Display}");DebuggerDisplay("Header: {Display}")>]
type Header =
     | IncludeHeader of Include
     | NamespaceHeader of Namespace with
        member this.Display
            with get() = 
                match this with
                | IncludeHeader i -> sprintf "%A" i
                | NamespaceHeader n -> sprintf "%A" n

type Document = { Headers:Header list; Definitions:Definition list }