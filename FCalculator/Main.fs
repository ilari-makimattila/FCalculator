module FCalculator.Main

open System

/// A node represents an operation in the formula
type Node =
    | Value of decimal
    | QuotedString of string
    | Variable of string
    | Date of string
    | Function of string * Node list
    | Add of Node * Node
    | Substract of Node * Node
    | Multiply of Node * Node
    | Divide of Node * Node
    | Modulo of Node * Node
    | Equality of Node * Node
    | Inequality of Node * Node
    | GreaterThan of Node * Node
    | LesserThan of Node * Node
    | GreaterOrEqualThan of Node * Node
    | LesserOrEqualThan of Node * Node
    | LogicalAnd of Node * Node
    | LogicalOr of Node * Node
