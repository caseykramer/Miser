module Ast

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
     | Map
     | List
     | Set

type BaseTypeValue = 
     | BoolValue of bool
     | ByteValue of byte
     | I16Value of int16
     | I32Value of int
     | I64Value of int64
     | DoubleValue of float
     | StringValue of string
     | BinaryValue of byte[]

type ContainerTypeValue = 
     | Map of Map<FieldType,FieldType>
     | List of FieldType list
     | Set of FieldType Set

and FieldType = 
     | IdentifierField of Identifier
     | BaseField of BaseTypeValue
     | ContainerField of ContainerTypeValue

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

type Document = Document of Header list * Definition list