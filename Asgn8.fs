// Definitions

// Abstract syntax definitions
type ExprC =
    // float is closest thing to Racket's "Real" type
    | NumC of float
    | StrC of string
    // No Symbol in F#, so string is used instead
    | IdC of string
    | IfC of ExprC * ExprC * ExprC
    | LamC of string list * ExprC
    | AppC of ExprC * ExprC list

// Values and environment definitions
type Value =
    | NumV of float
    | StrV of string
    | BoolV of bool
    | CloV of string list * ExprC * Env
    | PrimV of string
and Env = (string * Value) list

// Representation of a top-level environment
let topEnv : Env =
    [
        ("true", BoolV true)
        ("false", BoolV false)
        ("+", PrimV "+")
        ("-", PrimV "-")
        ("*", PrimV "*")
        ("/", PrimV "/")
        ("<=", PrimV "<=")
        ("equal?", PrimV "equal?")
        ("substring", PrimV "substring")
        ("strlen", PrimV "strlen")
        ("error", PrimV "error")
    ]

// Interpreter

// Parser (ignored for now, since F# has no sexps)

// Helpers


// Tests