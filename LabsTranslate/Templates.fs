module internal Templates
open Types
open Base


let indent num (s:string) = 
    s.Split "\n"
    |> Seq.map (sprintf "%s%s" (String.replicate num " "))
    |> String.concat "\n"

let translateKey (mapping:KeyMapping) index k = 
    let getTranslation index = function
    | I -> sprintf "I[%s][%i]" index mapping.[k].index
    | L -> sprintf "Lvalue[%s][%i]" index mapping.[k].index
    | E -> sprintf "E[%i]" mapping.[k].index

    let tryKeyInfo = mapping.TryFind k
    match tryKeyInfo with
    | Some(info) -> getTranslation index info.location
    | None -> failwith ("Unexpected key " + k)

let updateKq keys = 
    (keys
    |> Seq.map (fun x -> sprintf "setHin(tid, %i);" x)
    |> String.concat "\n") + "\n"

let assume = sprintf "__VERIFIER_assume(%s);\n"
let assertion = sprintf "assert(%s);\n"
let inlineassertion = sprintf "assert(%s);"

let entrypoint entry =
    assume <| (sprintf "pc[tid][%i] == %i" entry.pc entry.value)

let exitpoint exit =
    sprintf "pc[tid][%i] = %i;\n" exit.pc exit.value

let translatePoint = sprintf "packTuple(%i, %i)"

let signature = sprintf "%s_%i%s"

let attr = 
    sprintf """
int val = %s;
attr(tid, %i, val);
"""

let lstig = 
    sprintf """
int val = %s;
lstig(tid, %i, val);
"""

let env =
    sprintf """
int val = %s;
env(%i, val);
"""

let cfunc retType fName args body =
    (sprintf "%s %s(%s) {\n%s\n}\n") retType fName args body

let cvoid = cfunc "void"

let forLoop i j s = 
    (indent 4 s) |>
    (sprintf """
for (i=%i; i<%i; i++) {
%s
}
""" i j)

let arrayname keyInfo = 
        match keyInfo.location with
        | I -> "I[i]"
        | L -> "Lvalue[i]"
        | E -> "E"
let initLtstamp keyInfo = 
    match keyInfo.location with 
    | L -> sprintf "Ltstamp[i][%i] = j++;" keyInfo.index
    | _ -> ""

let typeofVar =
    function
    | (a,b) when a >= 0     && b < 256      -> "unsigned char "
    | (a,b) when a > -128   && b < 128      -> "char "
    | (a,b) when a >= 0     && b < 65536    -> "unsigned short "
    | (a,b) when a > -32768 && b < 32768    -> "short "
    | (a,b) when a >=0                      -> "unsigned int "
    | _ -> "int "

let def name = 
    function
    | ChooseI(l) -> (typeofVar (Seq.min l, List.max l) ) + name + ";"
    | RangeI(minI, maxI) -> (typeofVar (minI, maxI) + name + ";")

let initSimulate i keyInfo values =
    let initLtstamp i keyInfo = 
        match keyInfo.location with 
        | L -> sprintf "Ltstamp[%i][%i] = j++;" i keyInfo.index
        | _ -> ""

    let arrayname i keyInfo = 
            match keyInfo.location with
            | I -> sprintf "I[%i]" i
            | L -> sprintf "Lvalue[%i]" i
            | E -> "E"


    let guess = sprintf "guess%i%A" keyInfo.index keyInfo.location
    let rng = System.Random()
    sprintf "%s[%i] = %i;\n%s" (arrayname i keyInfo) keyInfo.index
    //(def guess values) + " = " + (
        (match values with
        | ChooseI(l) -> l.Item (rng.Next l.Length)
        | RangeI(minI, maxI) -> rng.Next(minI, maxI))
        (initLtstamp i keyInfo)

let init keyInfo values =
    let guess = sprintf "guess%i%A" keyInfo.index keyInfo.location

    let assumeInt = sprintf "(%s == %i)" guess
    let assumeIntRange key minI maxI =
        sprintf "%s >= %i && %s < %i" guess minI guess maxI
        |> assume

    let assign values =
        sprintf "%s[%i] = %s;\n%s" (arrayname keyInfo) keyInfo.index guess (initLtstamp keyInfo)

    (def guess values) + "\n" + (
        match values with
        | ChooseI(l) when l.Length = 1 -> 
            sprintf "%s = %i;\n" guess l.Head
        | ChooseI(l) -> 
            l
            |> Seq.map (assumeInt)
            |> String.concat " || "
            |> assume
        | RangeI(minI, maxI) -> assumeIntRange keyInfo.index minI maxI)
        + (assign values) 

let tmain typeofInterleaving body finallyProperties = 
    body
    |> typeofInterleaving
    |> indent 4 
    |> sprintf """
int main(void) {
    init();
    unsigned char choice[BOUND];
    int __LABS_step;
%s
%s
}
""" <| (indent 4 finallyProperties)

let fullInterleaving body = 
    body
    |> indent 8
    |> sprintf """
for (__LABS_step=0; __LABS_step<BOUND; __LABS_step++) {
    if (all_term()) break;

    __VERIFIER_assume(choice[__LABS_step] < MAXCOMPONENTS + 2);

    if (choice[__LABS_step] < MAXCOMPONENTS) {
%s
    }
    else if (choice[__LABS_step] == MAXCOMPONENTS) 
        propagate();
    else if (choice[__LABS_step] == MAXCOMPONENTS + 1)
        confirm();
    monitor();
}
"""

let fairInterleaving body = 
    body
    |> indent 8
    |> sprintf """
unsigned char last;
for (__LABS_step=0; __LABS_step<BOUND; __LABS_step++) {
    if (all_term()) break;

    __VERIFIER_assume(choice[__LABS_step] < MAXCOMPONENTS + 2);

    if (choice[__LABS_step] < MAXCOMPONENTS) {
        __VERIFIER_assume(choice[__LABS_step] == last+1 || (last == MAXCOMPONENTS - 1 && choice[__LABS_step] == 0));
%s
        last = choice[__LABS_step];
    }
    else if (choice[__LABS_step] == MAXCOMPONENTS) 
        propagate();
    else if (choice[__LABS_step] == MAXCOMPONENTS + 1)
        confirm();
    monitor();
}
"""


let baseHeader = """
#define MAX 256
#define undef_value 0x7FFFFFFF // MaxInt


int abs(int x) {
  int result = (x>0)?x:-x;
  return result;
}

int mod(int n, int m) {
  n = n >= 0 ? n % m : m + (n % m);
  return n;
}

int I[MAXCOMPONENTS][MAXKEYI];
int Lvalue[MAXCOMPONENTS][MAXKEYL];
int Ltstamp[MAXCOMPONENTS][MAXKEYL];

unsigned char isTuple[MAXKEYL];
unsigned char tupleStart[MAXKEYL];
unsigned char tupleEnd[MAXKEYL];

unsigned int Hin[MAXCOMPONENTS][MAXKEYL];
unsigned int Hout[MAXCOMPONENTS][MAXKEYL]; 
unsigned char HinCnt[MAXCOMPONENTS];
unsigned char HoutCnt[MAXCOMPONENTS];
unsigned char term[MAXCOMPONENTS];
int pc[MAXCOMPONENTS][MAXPC];
int E[MAXKEYE];
int __LABS_t;
"""

let systemFunctions = """
int now(void) {
    __LABS_t = __LABS_t+1;
    return __LABS_t;
}

void setHin(int id, int key) {
    if (Hin[id][key] == 0) {
        Hin[id][key] = 1;
        HinCnt[id] = HinCnt[id] + 1;
    }
}

void setHout(int id, int key) {
    if (Hout[id][key] == 0) {
        Hout[id][key] = 1;
        HoutCnt[id] = HoutCnt[id] + 1;
    }
}

//
//  Rule ATTR
//  Component component_id  assigns to key the evaluated expression
//
void attr(int component_id, int key, int value) {
    __VERIFIER_assume(HoutCnt[component_id] == 0);
    __VERIFIER_assume(HinCnt[component_id] == 0);
    I[component_id][key] = value;
    now(); // local step
}

//
//  Rule LSTIG
//
void lstig(int component_id, int key, int value) {
    __VERIFIER_assume(HoutCnt[component_id] == 0);
    __VERIFIER_assume(HinCnt[component_id] == 0);

    Lvalue[component_id][key] = value;
    int k;
    for (k = 0; k < MAXKEYL; k++) {
        if (k >= tupleStart[key] && k <= tupleEnd[key]) {
            Ltstamp[component_id][k] = now();
        }
    }

    setHout(component_id, key);
}

unsigned char differentLstig(int comp1, int comp2, int key) {
    unsigned char k;
    for (k = 0; k < MAXKEYL; k++) {
        if (k >= tupleStart[key] && k <= tupleEnd[key]) {
            if (Lvalue[comp1][k] != Lvalue[comp1][k] || (Ltstamp[comp1][k] != Ltstamp[comp2][k])) {
                return 1;
            }
        }
    }
    return 0;
}

void confirm(void) {
    unsigned char guessedcomp;
    __VERIFIER_assume(guessedcomp < MAXCOMPONENTS);
    __VERIFIER_assume(HinCnt[guessedcomp] > 0);
    // __VERIFIER_assume(HoutCnt[guessedcomp] == 0); // Priority to propagate()

    unsigned char guessedkey;
    __VERIFIER_assume(guessedkey < MAXKEYL);
    __VERIFIER_assume(Hin[guessedcomp][guessedkey] == 1);

    int i, k;
    int t = Ltstamp[guessedcomp][guessedkey];
    ////printf(">>>[%d] start Hin (%d)\n", guessedcomp, guessedkey);    
    
    for (i=0; i<MAXCOMPONENTS; i++) {
        if ( (guessedcomp!=i) && link(guessedcomp,i) && differentLstig(guessedcomp, i, guessedkey) ) {
            setHout(i, guessedkey);
            for (k = 0; k < MAXKEYL; k++) {
                if (k >= tupleStart[guessedkey] && k <= tupleEnd[guessedkey]) {
                    if (Ltstamp[i][k]<=t) {
                        Lvalue[i][k] = Lvalue[guessedcomp][k];
                        Ltstamp[i][k] = t;
                    }
                }
            }
        }
    }

    Hin[guessedcomp][guessedkey] = 0;
    HinCnt[guessedcomp]--;
}

void propagate(void) {
    unsigned char guessedcomp;
     __VERIFIER_assume(guessedcomp < MAXCOMPONENTS);
     __VERIFIER_assume(HoutCnt[guessedcomp] > 0);
     // __VERIFIER_assume(HinCnt[guessedcomp] == 0); // Priority to Confirm()

    unsigned char guessedkey;
    __VERIFIER_assume(guessedkey < MAXKEYL);
    __VERIFIER_assume(Hout[guessedcomp][guessedkey] == 1);

    int i, k;
    int t = Ltstamp[guessedcomp][guessedkey];

    for (i=0; i<MAXCOMPONENTS; i++) {

        if ((guessedcomp!=i) && (link(guessedcomp,i)) && (Ltstamp[i][guessedkey]<t)) {
            for (k = 0; k < MAXKEYL; k++) {
                if (k >= tupleStart[guessedkey] && k <= tupleEnd[guessedkey]) {
                    Lvalue[i][k] = Lvalue[guessedcomp][k];
                    Ltstamp[i][k] = t;
                }
            }
            setHout(i, guessedkey);
        }
    }

    Hout[guessedcomp][guessedkey] = 0;
    HoutCnt[guessedcomp] = HoutCnt[guessedcomp] - 1;
}

char all_term() {
    int i;
    for (i=0; i<MAXCOMPONENTS; i++) {
        if (term[i] == 0) return 0;
    }
    return 1;
}
"""

let baseInit = sprintf """
int i,j;
for (i=0; i<MAXKEYE; i++) {
        E[i] = undef_value;
    }
for (i=0; i<MAXCOMPONENTS; i++) {
    term[i] = 0;
    for (j=0; j<MAXKEYI; j++) {
        I[i][j] = undef_value;
    }
    for (j=0; j<MAXKEYI; j++) {
        I[i][j] = undef_value;
    }
    for (j=0; j<MAXKEYL; j++) {
        Lvalue[i][j] = undef_value;
        Ltstamp[i][j] = 0;
        Hin[i][j] = 0;
        Hout[i][j] = 0;
    }
    for (j=0; j<MAXPC; j++) {
        pc[i][j] = 0;
    } 
    HinCnt[i] = 0;
    HoutCnt[i] = 0;
}
j=0;
%s
__LABS_t = j;

for (i=0; i<MAXCOMPONENTS; i++) {
    for (j=0; j<MAXKEYL; j++) {
        Ltstamp[i][j] = Ltstamp[i][tupleEnd[j]];
    }
}

"""

let resetPcs = """int i;
for (i=1; i<MAXPC; i++) {
    pc[tid][i] = 0;
}
"""

let serializeInfo (sys, mapping:KeyMapping) =
    let serializeKeys (m:KeyMapping) =
        if m.Count = 0 then ""
        else
        m
        |> Map.toSeq
        |> Seq.sortBy (fun (_,info) -> info.index)
        |> Seq.map (fun (name, _) -> name)
        |> String.concat ","

    let maxTupleLength =
        sys.components
        |> Map.map (fun _ cdef -> cdef.lstig |> List.map (fun m -> m.Count))
        |> Map.values
        |> fun x -> if Seq.isEmpty x then Seq.empty else (Seq.concat x)
        |> fun x -> if Seq.isEmpty x then 0 else Seq.max x

    let ranges = 
        sys.spawn
        |> Map.map (fun k (cmin, cmax) -> sprintf "%s %i,%i" k cmin cmax)
        |> Map.values
        |> fun x -> if Seq.isEmpty x then "" else String.concat ";" x

    [|I;L;E|]
    |> Seq.map (fun t -> 
        Map.filter (fun _ info -> info.location = t) mapping
        |> serializeKeys)
    |> fun x -> if Seq.isEmpty x then "" else String.concat "\n" x
    |> fun s -> printfn "%s\n%s\nunwind %i" s ranges (maxTupleLength + 1)

    Result.Ok(0)