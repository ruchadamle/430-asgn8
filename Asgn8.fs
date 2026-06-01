// --- DEFINITIONS ---
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

// --- HELPERS ---
// Looks up a variable name in the environment
let lookup (name : string) (env : Env) : Value =
    match List.tryFind (fun (n, v) -> n = name) env with
    | Some (_, v) -> v
    | None -> failwith ("VEBG unbound name: " + name)

// Converts a VEBG value into a string
let serialize (value : Value) : string =
    match value with
    | NumV n -> string n
    | StrV s -> "\"" + s + "\""
    | BoolV true -> "true"
    | BoolV false -> "false"
    | CloV _ -> "#<procedure>"
    | PrimV _ -> "#<primop>"

// Checks if two values are equal and prints the result
let checkEqual (name : string) (actual : Value) (expected : Value) =
    if actual = expected then
        printfn "PASS: %s" name
    else
        printfn "FAIL: %s\nactual:%A\nexpected: %A" name actual expected

// Checks that some code throws an error containing "VEBG"
let checkExn (name : string) (exnTest : unit -> unit) =
    try
        exnTest ()
        printfn "FAIL: %s" name
    with
    | ex ->
        if ex.Message.Contains("VEBG") then
            printfn "PASS: %s" name
        else
            printfn "FAIL: %s" name

// --- Interpreter ---
// Interprets an ExprC
let rec interp (expr : ExprC) (env : Env) : Value =
    match expr with
    | NumC n -> NumV n
    | StrC s -> StrV s
    | IfC (t, th, el) -> match interp t env with
                                            | BoolV true -> interp th env
                                            | BoolV false -> interp el env
                                            | _ -> failwith "VEBG expected boolean in if"
    | LamC (paramNames, body) -> CloV (paramNames, body, env)
    | AppC (fpos, args) -> failwith "VEBG stub not implemented NEED TO TEST LATER WHEN IMPLENETED"
    | IdC name -> lookup name env

// --- PARSER --- (ignored for now, since F# has no sexps)

// --- TESTS ---
// Lookup tests
printfn("--- Lookup Tests ---")
checkEqual "lookup true" (lookup "true" topEnv)(BoolV true)
checkEqual "lookup false" (lookup "false" topEnv)(BoolV false)
checkEqual "lookup plus" (lookup "+" topEnv)(PrimV "+")
checkEqual "lookup x" (lookup "x" [ ("x", NumV 5.0) ])(NumV 5.0)
// Test named "lookup missing name" that makes a lambda that calls lookup on 
// a name that doesn't exist in the env, and checks that it throws an error containing "VEBG"
checkExn "lookup missing name" (fun () -> lookup "missing" topEnv |> ignore)

// Serialize tests
printfn("\n--- Serialize Tests ---")
checkEqual "serialize number" (StrV (serialize (NumV 5.0))) (StrV "5")
checkEqual "serialize string" (StrV (serialize (StrV "hi"))) (StrV "\"hi\"")
checkEqual "serialize true" (StrV (serialize (BoolV true))) (StrV "true")
checkEqual "serialize false" (StrV (serialize (BoolV false))) (StrV "false")
checkEqual "serialize prim" (StrV (serialize (PrimV "+"))) (StrV "#<primop>")
checkEqual "serialize closure" (StrV (serialize (CloV (["x"], IdC "x", [])))) (StrV "#<procedure>")

// Interp tests
printfn("\n--- Interp Tests ---")
checkEqual "interp number" (interp (NumC 5.0) topEnv) (NumV 5.0)
checkEqual "interp string" (interp (StrC "hi") topEnv) (StrV "hi")
checkEqual "interp true" (interp (IdC "true") topEnv) (BoolV true)
checkEqual "interp false" (interp (IdC "false") topEnv) (BoolV false)
checkEqual "interp if true" (interp (IfC (IdC "true", NumC 1.0, NumC 2.0)) topEnv) (NumV 1.0)
checkEqual "interp if false" (interp (IfC (IdC "false", NumC 1.0, NumC 2.0)) topEnv) (NumV 2.0)
checkExn "interp if given non-boolean test" (fun () -> interp (IfC (NumC 0.0, NumC 1.0, NumC 2.0)) topEnv |> ignore)
checkEqual "interp function" (interp (LamC (["x"], IdC "x")) topEnv) (CloV (["x"], IdC "x", topEnv))
