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

let entrypoint pc entry =
    assume <| (sprintf "pc[tid][%i] == %i" pc entry)

let exitpoint =
    sprintf "pc[tid][%i] = %i;\n"


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
E[%i] = val;
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

let init keyInfo values =
    let guess = sprintf "guess%i%A" keyInfo.index keyInfo.location

    let arrayname = 
        match keyInfo.location with
        | I -> "I[i]"
        | L -> "Lvalue[i]"
        | E -> "E"
    let initLtstamp = 
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

    //let nameP = sprintf "%sx, %sy;" guess guess

    let assumeInt = sprintf "(%s == %i)" guess
    //let assumeP = sprintf "(%sx == %i && %sy == %i)" 
    let assumeIntRange key minI maxI =
        sprintf "%s >= %i && %s < %i" guess minI guess maxI
        |> assume
    //let assumePRange key (xmin, ymin) (xmax, ymax)= 
        //(sprintf "(%sx >= %i && %sx < %i) && (%sy >= %i && %sy < %i)"
        //guess xmin guess xmax guess ymin guess ymax)
        //|> assume

    let def = 
        function
        | ChooseI(l) -> (typeofVar (Seq.min l, List.max l) ) + guess + ";"
        | RangeI(minI, maxI) -> (typeofVar (minI, maxI) + guess + ";")

    let assign values =
        sprintf "%s[%i] = %s;\n%s" arrayname keyInfo.index guess initLtstamp

    (def values) + "\n" + (values
    |> (function
        | ChooseI(l) when l.Length = 1 -> 
            sprintf "%s = %i;\n" guess l.Head
        | ChooseI(l) -> 
            l
            |> Seq.map (assumeInt)
            |> String.concat " || "
            |> assume
        | RangeI(minI, maxI) -> assumeIntRange keyInfo.index minI maxI)
    ) + (assign values) 

let tmain typeofInterleaving body finallyProperties = 
    body
    |> typeofInterleaving
    |> indent 4 
    |> sprintf """
int main(void) {
    init();
    unsigned char choice[BOUND];
    int i;
%s
%s
}
""" <| (indent 4 finallyProperties)

let fullInterleaving body = 
    body
    |> indent 8
    |> sprintf """
for (i=0; i<BOUND; i++) {
    __VERIFIER_assume(choice[i] < MAXPROCS + 2);

    if (choice[i] < MAXPROCS) {
%s
    }
    else if (choice[i] == MAXPROCS) 
        propagate();
    else if (choice[i] == MAXPROCS + 1)
        confirm();
    monitor();
}
"""

let fairInterleaving body = 
    body
    |> indent 8
    |> sprintf """
unsigned char last;
for (i=0; i<BOUND; i++) {

    __VERIFIER_assume(choice[i] < MAXPROCS + 2);

    if (choice[i] < MAXPROCS) {
        __VERIFIER_assume(choice[i] == last+1 || (last == MAXPROCS - 1 && choice[i] == 0));
%s
        last = choice[i];
    }
    else if (choice[i] == MAXPROCS) 
        propagate();
    else if (choice[i] == MAXPROCS + 1)
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

int I[MAXPROCS][MAXKEYI];
int Lvalue[MAXPROCS][MAXKEYL];
int Ltstamp[MAXPROCS][MAXKEYL];

unsigned char isTuple[MAXKEYL];
unsigned char tupleStart[MAXKEYL];
unsigned char tupleEnd[MAXKEYL];

unsigned int Hin[MAXPROCS][MAXKEYL];
unsigned int Hout[MAXPROCS][MAXKEYL]; 
unsigned char HinCnt[MAXPROCS];
unsigned char HoutCnt[MAXPROCS];
int pc[MAXPROCS][MAXPC];
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
    for (k = tupleStart[key]; k <= tupleEnd[key]; k++) {
        Ltstamp[component_id][k] = now();
    }

    setHout(component_id, key);
}

void confirm(void) {
    unsigned char guessedcomp;
    __VERIFIER_assume(guessedcomp < MAXPROCS);
    __VERIFIER_assume(HinCnt[guessedcomp] > 0);
    // __VERIFIER_assume(HoutCnt[guessedcomp] == 0); // Priority to propagate()

    unsigned char guessedkey;
    __VERIFIER_assume(guessedkey < MAXKEYL);
    __VERIFIER_assume(Hin[guessedcomp][guessedkey] == 1);

    int i, k;
    int t = Ltstamp[guessedcomp][guessedkey];
    ////printf(">>>[%d] start Hin (%d)\n", guessedcomp, guessedkey);    
    
    for (i=0; i<MAXPROCS; i++) {
        if ( (guessedcomp!=i) && link(guessedcomp,i) ) {
            setHout(i, guessedkey);
            for (k = tupleStart[guessedkey]; k <= tupleEnd[guessedkey]; k++) {
                if (Ltstamp[i][k]<=t) {
                    Lvalue[i][k] = Lvalue[guessedcomp][k];
                    Ltstamp[i][k] = t;
                }
            }
        }
    }

    Hin[guessedcomp][guessedkey] = 0;
    HinCnt[guessedcomp]--;
}

void propagate(void) {
    unsigned char guessedcomp;
     __VERIFIER_assume(guessedcomp < MAXPROCS);
     __VERIFIER_assume(HoutCnt[guessedcomp] > 0);
     // __VERIFIER_assume(HinCnt[guessedcomp] == 0); // Priority to Confirm()

    unsigned char guessedkey;
    __VERIFIER_assume(guessedkey < MAXKEYL);
    __VERIFIER_assume(Hout[guessedcomp][guessedkey] == 1);

    int i, k;
    int t = Ltstamp[guessedcomp][guessedkey];

    for (i=0; i<MAXPROCS; i++) {

        if ((guessedcomp!=i) && (link(guessedcomp,i)) && (Ltstamp[i][guessedkey]<t)) {
            for (k = tupleStart[guessedkey]; k <= tupleEnd[guessedkey]; k++) {
                Lvalue[i][k] = Lvalue[guessedcomp][k];
                Ltstamp[i][k] = t;
            }
            setHout(i, guessedkey);
        }
    }

    Hout[guessedcomp][guessedkey] = 0;
    HoutCnt[guessedcomp] = HoutCnt[guessedcomp] - 1;
}
"""

let baseInit = sprintf """
int i,j;
for (i=0; i<MAXKEYE; i++) {
        E[i] = undef_value;
    }
for (i=0; i<MAXPROCS; i++) {
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

for (i=0; i<3; i++) {
    for (j=0; j<MAXKEYL; j++) {
        Ltstamp[i][j] = Ltstamp[i][tupleEnd[j]];
    }
}

"""


let toJson (spawn: Map<string, int*int>) (mapping:KeyMapping) =
    //let convertType = function
    //| Int(_) -> "\"int\""
    //| P(_) -> "\"point\""

    let ranges = 
        spawn
        |> Map.map (fun k (cmin, cmax) -> sprintf "\"%s\": [%i,%i]" k cmin cmax)
        |> Map.values |> String.concat ","
    let keyNames =
        mapping
        |> Map.toSeq
        |> Seq.sortBy (fun (_,info) -> info.index)
        |> Seq.map (fun (name, _) -> sprintf "\"%s\"" name)
        |> String.concat ","

    sprintf """{
    "ranges": {%s}
    "keyNames": [%s]
}""" ranges keyNames
    |> eprintfn "%s"