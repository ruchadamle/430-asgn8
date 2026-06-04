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
// F# supports tuples so no need for separate Binding type 
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

//used to exntend the environment 
let extendEnv (paramNames : string list) (argVals : Value list) (env : Env) : Env =
    List.zip paramNames argVals @ env

let checkStringEqual (name : string) (actual : string) (expected : string) =
    if actual = expected then
        printfn "PASS: %s" name
    else
        printfn "FAIL: %s\nactual:%s\nexpected:%s" name actual expected

// Runs all primitive operations
let primOps (op : string) ( args : Value list) : Value =
    match op, args with 
    | "+", [NumV a; NumV b] -> NumV (a + b)
    | "-", [NumV a; NumV b] -> NumV (a - b)
    | "*", [NumV a; NumV b] -> NumV (a * b)
    | "/", [NumV a; NumV b] -> NumV (a / b)
    | "<=", [NumV a; NumV (b: float)] -> BoolV (a <= b)
    | "<=", _ -> failwith "VEBG <=: expected two numbers"
    | "equal?", [a; b] ->
        match a, b with
        | (CloV _ | PrimV _), _ -> BoolV false
        | _, (CloV _ | PrimV _) -> BoolV false
        | NumV x, NumV y -> BoolV (x = y)
        | StrV x, StrV y -> BoolV (x = y)
        | BoolV x, BoolV y -> BoolV (x = y)
        | _ -> BoolV false
    | "equal?", _ -> failwith "VEBG equal?: expected two arguments"
    | "substring", [StrV s; NumV start; NumV stop] ->
        let isNat (x : float) = x >= 0.0 && x = floor x
        if not (isNat start) || not (isNat stop) then
            failwith "VEBG substring: start and stop must be naturals"
        else
            let st = int start
            let sp = int stop
            if st > sp then
                failwith "VEBG substring: start is after stop"
            elif sp > s.Length then
                failwith "VEBG substring: index out of range"
            else
                StrV (s.Substring(st, sp - st))
    | "substring", _ -> failwith "VEBG substring: expected (string, natural, natural)"
    | "strlen", [StrV s] -> NumV (float s.Length)
    | "strlen", _ -> failwith "VEBG strlen: expected a string"
    | "error", [v] -> failwith ("VEBG user-error: " + serialize v)
    | "error", _ -> failwith "VEBG error: expected one argument"
    | _ -> failwith "primOps other: primitive not found"

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
    | AppC (fpos, args) -> 
        match interp fpos env with 
        | CloV (paramNames, body, cloEnv ) -> 
            if List.length paramNames = List.length args 
            then 
                let argVals = List.map (fun arg -> interp arg env) args 
                let newEnv = extendEnv paramNames argVals cloEnv
                interp body newEnv 
            else failwith "AppC CloV arity failed"
        | PrimV op ->  
            let argVals =  List.map (fun arg -> interp arg env) args //map interp on each arg of args
            primOps op argVals
        | other -> failwith "AppC other: unsported AppC argument"
    | IdC name -> lookup name env

// --- PARSER --- (ignored for now, since F# has no sexps)

// --- TESTS ---
// For testing, there are multiple helpers:
// - checkEqual: for checking that two values are equal
// - checkStringEqual: for checking that two strings are equal
// - checkExn: for checking that some code throws an error containing "VEBG"
//
// Use checkStringEqual for testing serialize and topInterp
// Use checkEqual for testing pretty much evrything else

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
checkStringEqual "serialize number" (serialize (NumV 5.0)) "5"
checkStringEqual "serialize string" (serialize (StrV "hi")) "\"hi\""
checkStringEqual "serialize true" (serialize (BoolV true)) "true"
checkStringEqual "serialize false" (serialize (BoolV false)) "false"
checkStringEqual "serialize prim" (serialize (PrimV "+")) "#<primop>"
checkStringEqual "serialize closure" (serialize (CloV (["x"], IdC "x", []))) "#<procedure>"

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
checkEqual "interp AppC PrimV" (interp (AppC (IdC "+", [NumC 1.0; NumC 2.0])) topEnv) (NumV 3.0)
checkEqual "interp AppC LamC" (interp (AppC (LamC (["x"], IdC "x"), [NumC 10.0])) topEnv) (NumV 10.0)
