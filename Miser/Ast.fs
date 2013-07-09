module Ast

type Comment = 
     | Comment of string
     | CommentBlock of string list

type Identifier = Identifier of string

type Literal = 
     | StringLiteral of string
     | NumericLiteral of int64
     | DecimalLiteral of float

type ConstantValue =
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
     | SList

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
     | Field of FieldType*Identifier

type Exception = 
     | Exception of Field list

type Function = 
     | OnewayFunction of Function
     | VoidFunction of Identifier * Field list * Exception option
     | Function of FieldType * Identifier * Field list * Exception option

type Service = 
     | InheritsService of Service
     | Service of Identifier * Function list

type Struct = 
     | Struct of Identifier * Field list

type StringEnum = 
     | SEnum of Identifier*Literal list

type Enum = 
     | Enum of Identifier*(Literal*int) list

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

type NamespaceScope = 
     | Any
     | Cpp
     | Java
     | Py
     | Perl
     | Rb
     | Cocoa
     | CSharp

type Namespace = 
     Namespace of NamespaceScope * Identifier

type Include = 
     | Include of Literal

type Header =
     | IncludeHeader of Include
     | NamespaceHeader of Namespace

type Document = { Headers:Header list; Definitions:Definition list }