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
    | None -> failwith "Unexpected key " + k

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
attr(tid, %i, val, -1);
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
    let guess = sprintf "guess%i" keyInfo.index

    let arrayname = 
        match keyInfo.location with
        | I -> "I[i]"
        | L -> "Lvalue[i]"
        | E -> "E"
    let initLtstamp = 
        match keyInfo.location with 
        | L -> sprintf "Ltstamp[i][%i] = (i+1) * (%i + 1);" keyInfo.index keyInfo.index
        | _ -> "" 


    let typeofVar =
        function
        | (a,b) when a >= 0     && b < 256      -> "unsigned char "
        | (a,b) when a > -128   && b < 128      -> "char "
        | (a,b) when a >= 0     && b < 65536    -> "unsigned short "
        | (a,b) when a > -32768 && b < 32768    -> "short "
        | (a,b) when a >=0                      -> "unsigned int "
        | _ -> "int "

    let nameP = sprintf "%sx, %sy;" guess guess

    let assumeInt = sprintf "(%s == %i)" guess
    let assumeP = sprintf "(%sx == %i && %sy == %i)" 
    let assumeIntRange key minI maxI =
        sprintf "%s >= %i && %s < %i" guess minI guess maxI
        |> assume
    let assumePRange key (xmin, ymin) (xmax, ymax)= 
        (sprintf "(%sx >= %i && %sx < %i) && (%sy >= %i && %sy < %i)"
        guess xmin guess xmax guess ymin guess ymax)
        |> assume

    let def = 
        function
        | ChooseI(l) -> (typeofVar (Seq.min l, List.max l) ) + guess + ";"
        | ChooseP(l) ->
            let xs = l |> Seq.map fst
            let ys = l |> Seq.map snd
            let minval = min (Seq.min xs) (Seq.min ys)
            let maxval = max (Seq.max xs) (Seq.max ys)
            (typeofVar (minval, maxval) + nameP)
        | RangeI(minI, maxI) -> (typeofVar (minI, maxI) + guess + ";")
        | RangeP((xmin, ymin), (xmax,ymax)) -> 
            let minval = min xmin ymin
            let maxval = max xmax ymax
            (typeofVar (minval, maxval) + nameP)
    let assign values =
        let prefix = 
            match values with
            | ChooseP(_)
            | RangeP(_) -> 
                (sprintf "int %s = packTuple(%sx, %sy);" guess guess guess)
            | _ -> ""
        sprintf "%s\n%s[%i] = %s;\n%s" prefix arrayname keyInfo.index guess initLtstamp

    (def values) + "\n" + (values
    |> (function
        | ChooseI(l) -> 
            l
            |> Seq.map (assumeInt)
            |> String.concat " || "
            |> assume
        | ChooseP(ps) -> 
            ps 
            |> Seq.map (fun (x,y) ->(assumeP guess x guess y))
            |> String.concat " || "
            |> assume
        | RangeI(minI, maxI) -> assumeIntRange keyInfo.index minI maxI
        | RangeP(minP, maxP) -> assumePRange keyInfo.index minP maxP
       )
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

int isNegative(int x) {
  int result = (x>=0)?0:1;
  return result;
}

int mod(int n, int m) {
  n = n >= 0 ? n % m : m + (n % m);
  return n;
}

int packTuple(char x, char y) {
  int tup = (abs(y) % MAX) + ((abs(x) % MAX) * MAX);
  int signX = isNegative(x) * 131072; // << 17
  int signY = isNegative(y) * 65536; // << 16
  return tup + signX + signY;
}

char getX(int tup) {
  char result = (char) ((tup % 65536) / MAX);
  char negative = (char) (tup / 131072) % 2;
  return negative == 1 ? -result : result;
}

char getY(int tup) {
  char result = (char) (tup % MAX);
  char negative = (char) (tup / 65536) % 2;
  return negative == 1 ? -result : result;
}

int sumTuple(int t1, int t2) {
  int result = packTuple((char) getX(t1)+getX(t2), (char) getY(t1)+getY(t2));
  return result;
}

int minusTuple(int t1, int t2) {
  int result = packTuple((char) getX(t1)-getX(t2), (char) getY(t1)-getY(t2));
  return result;
}

int modTuple(int t1, int tmod) {
  char x = mod(getX(t1), getX(tmod));
  char y = mod(getY(t1), getY(tmod));
  int result = packTuple(x, y);
  return result;
}

int d2Tuple(int t) {
    char x = getX(t);
    char y = getY(t);
    return (unsigned int) x*x + y*y;
}

int I[MAXPROCS][MAXKEY];
int Lvalue[MAXPROCS][MAXKEY];
int Ltstamp[MAXPROCS][MAXKEY];
unsigned int Hin[MAXPROCS][MAXKEY];
unsigned int Hout[MAXPROCS][MAXKEY]; 
unsigned char HinCnt[MAXPROCS];
unsigned char HoutCnt[MAXPROCS];
int pc[MAXPROCS][MAXPC];
int E[MAXKEY];
int currenttimestamp;
"""

let systemFunctions = """unsigned char differentLstig(int comp1, int comp2, int key){
    unsigned char result;
    result = (Lvalue[comp1][key] != Lvalue[comp2][key]) || (Ltstamp[comp1][key] != Ltstamp[comp1][key]);
    return result; 
}

int now(void) {
    currenttimestamp = currenttimestamp+1;
    return currenttimestamp;
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
void attr(int component_id, int key, int value, int keyinexpression) {
    __VERIFIER_assume(HoutCnt[component_id] == 0);
    __VERIFIER_assume(HinCnt[component_id] == 0);
    I[component_id][key] = value;
    now(); // local step
    if (keyinexpression!=-1) {
        setHin(component_id, keyinexpression);
    }
}

//
//  Rule LSTIG
//
void lstig(int component_id, int key, int value, int keyinexpression) {
    __VERIFIER_assume(HoutCnt[component_id] == 0);
    __VERIFIER_assume(HinCnt[component_id] == 0);
    Lvalue[component_id][key] = value;
    Ltstamp[component_id][key] = now();

    if ((keyinexpression!=-1)) {
        setHin(component_id, keyinexpression);
    }

    setHout(component_id, key);
}

void confirm(void) {
    unsigned char guessedcomp;
    __VERIFIER_assume(guessedcomp < MAXPROCS);
    __VERIFIER_assume(HinCnt[guessedcomp] > 0);
    // __VERIFIER_assume(HoutCnt[guessedcomp] == 0); // Priority to propagate()

    unsigned char guessedkey;
    __VERIFIER_assume(guessedkey < MAXKEY);
    __VERIFIER_assume(Hin[guessedcomp][guessedkey] == 1);

    int i;
    int t = Ltstamp[guessedcomp][guessedkey];
    ////printf(">>>[%d] start Hin (%d)\n", guessedcomp, guessedkey);    
    
    for (i=0; i<MAXPROCS; i++) {
        if ( (guessedcomp!=i) && link(guessedcomp,i) && (differentLstig(guessedcomp, i, guessedkey)) ) {
            if (Ltstamp[i][guessedkey]<=t) {
                Lvalue[i][guessedkey] = Lvalue[guessedcomp][guessedkey];
                Ltstamp[i][guessedkey] = t;
                setHout(i, guessedkey);
            }
            else { //(Ltstamp[i][guessedkey]>t)
                setHout(i, guessedkey);
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
    __VERIFIER_assume(guessedkey < MAXKEY);
    __VERIFIER_assume(Hout[guessedcomp][guessedkey] == 1);

    int i;
    int t = Ltstamp[guessedcomp][guessedkey];

    for (i=0; i<MAXPROCS; i++) {

        if ((guessedcomp!=i) && (link(guessedcomp,i)) && (Ltstamp[i][guessedkey]<t)) {
            Lvalue[i][guessedkey] = Lvalue[guessedcomp][guessedkey];
            Ltstamp[i][guessedkey] = t;
            setHout(i, guessedkey);
        }
    }

    Hout[guessedcomp][guessedkey] = 0;
    HoutCnt[guessedcomp] = HoutCnt[guessedcomp] - 1;
}
"""

let baseInit = sprintf """
int i,j;
for (i=0; i<MAXPROCS; i++) {
    for (j=0; j<MAXKEY; j++) {
        I[i][j] = undef_value;
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
%s
currenttimestamp = MAXPROCS*MAXKEY + 2;
"""


let toJson (spawn: Map<string, int*int>) (mapping:KeyMapping) =
    let convertType = function
    | Int(_) -> "\"int\""
    | P(_) -> "\"point\""

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
    let keyTypes =
        mapping
        |> Map.toSeq
        |> Seq.sortBy (fun (_, info) -> info.index)    
        |> Seq.map (fun (_, info) -> (convertType info.typ))
        |> String.concat ","

    sprintf """{
    "ranges": {%s}
    "keyNames": [%s]
    "keyTypes": [%s]
}""" ranges keyNames keyTypes
    |> eprintf "%s"