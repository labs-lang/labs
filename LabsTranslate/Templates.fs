module internal Templates
open Types
open Base


let indent num (s:string) = 
    s.Split "\n"
    |> Seq.map (sprintf "%s%s" (String.replicate num " "))
    |> String.concat "\n"

let translateKey (mapping:Map<(string * TypeofKey), int>) index = function
| k when mapping.ContainsKey (k, I) ->
    sprintf "comp[%s].I[%i]" index mapping.[k,I]
| k when mapping.ContainsKey (k, L)  -> 
    sprintf "comp[%s].I[%i]" index mapping.[k,L]
| k when mapping.ContainsKey (k, E)  -> 
    sprintf "E[%i]" mapping.[k,E]
| k -> printf "%s" k; failwith "Unexpected key " + k

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
    (sprintf "%s %s (%s) {\n%s\n}\n") retType fName args body

let cvoid = cfunc "void"


let globals = sprintf """
component comp[MAXPROCS];
int pc[MAXPROCS][%i];
int currenttimestamp;
"""

let forLoop i j s = 
    (indent 4 s) |>
    (sprintf """
for (i=%i; i<%i; i++) {
%s
}
""" i j)


let init arrayname key values =
    let guess = sprintf "guess%i" key

    let typeofVar =
        function
        | (a,b) when a >= 0     && b < 256      -> "unsigned char "
        | (a,b) when a > -128   && b < 128      -> "char "
        | (a,b) when a >= 0     && b < 65536    -> "unsigned short "
        | (a,b) when a > -32768 && b < 32768    -> "short "
        | (a,b) when a >=0                      -> "unsigned int "
        | _ -> "int "

    let nameI = sprintf "guess%i;\n" key
    let nameP = sprintf "guess%ix, guess%iy;\n" key key

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
        | ChooseI(l) -> (typeofVar (Seq.min l, List.max l) ) + nameI
        | ChooseP(l) ->
            let xs = l |> Seq.map fst
            let ys = l |> Seq.map snd
            let minval = min (Seq.min xs) (Seq.min ys)
            let maxval = max (Seq.max xs) (Seq.max ys)
            (typeofVar (minval, maxval) + nameP)
        | RangeI(minI, maxI) -> (typeofVar (minI, maxI) + nameI)
        | RangeP((xmin, ymin), (xmax,ymax)) -> 
            let minval = min xmin ymin
            let maxval = max xmax ymax
            (typeofVar (minval, maxval) + nameP)
    let assign values =
        let prefix = 
            match values with
            | ChooseP(_)
            | RangeP(_) -> 
                (sprintf " int %s = packTuple(%sx, %sy);" guess guess guess)
            | _ -> ""
        "\n" + prefix + (sprintf "\n %s[%i] = %s;" arrayname key guess)

    (def values) + "\n" + (values
    |> (function
        | ChooseI(l) -> 
            l
            |> Seq.map (assumeInt)
            |> String.concat " | "
            |> assume
        | ChooseP(ps) -> 
            ps 
            |> Seq.map (fun (x,y) ->(assumeP guess x guess y))
            |> String.concat "|"
            |> assume
        | RangeI(minI, maxI) -> assumeIntRange key minI maxI
        | RangeP(minP, maxP) -> assumePRange key minP maxP
       )
    ) + (assign values) 


let tmain body finallyProperties = 
    (indent 12 body)
    |> sprintf """
int main(void) {
    init();
    unsigned char choice[BOUND];

    int i;
    for (i=0; i<BOUND; i++) {
        __VERIFIER_assume(choice[i] < MAXPROCS + 3);

        if (choice[i] < MAXPROCS) {
%s
        }
        else if (choice[i] == MAXPROCS) 
            propagate();
        else if (choice[i] == MAXPROCS + 1)
            confirm();
        else if (choice[i] == MAXPROCS + 2)
            monitor();
    }

%s
}
""" <| (indent 4 finallyProperties)


let baseHeader = """
#define MAX 256

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

int sum(int t1, int t2) {
  int result = packTuple((char) getX(t1)+getX(t2), (char) getY(t1)+getY(t2));
  return result;
}

int sum2(int t1, int t2, char m) {
  char sum1 = mod(getX(t1)+getX(t2), m);
  char sum2 = mod(getY(t1)+getY(t2), m);
  int result = packTuple(sum1, sum2);
  return result;
}

typedef struct _component {
    int I[MAXKEY];          // we assume integer values for all keys
    int Lvalue[MAXKEY];  
    int Ltstamp[MAXKEY];
    unsigned int Hin[MAXKEY];        // Hin[j] = 1 --> key j is in Hin
    unsigned int Hout[MAXKEY];       // Hout ...
    unsigned char HinCnt;
    unsigned char HoutCnt;
} component;
"""

let systemFunctions = """unsigned char differentLstig(int comp1, int comp2, int key){
    unsigned char result;
    result = (comp[comp1].Lvalue[key] != comp[comp2].Lvalue[key]) || (comp[comp1].Ltstamp[key] != comp[comp2].Ltstamp[key]);
    return result; 
}

int now(void) {
    currenttimestamp = currenttimestamp+1;
    return currenttimestamp;
}

void setHin(int id, int key) {
    if (comp[id].Hin[key] == 0) {
        comp[id].Hin[key] = 1;
        comp[id].HinCnt = comp[id].HinCnt + 1;
    }
}

#define _setHin(id, key) ((comp[id].Hin[key] == 0)?(comp[id].HinCnt+=comp[id].Hin[key]=1):0)

void setHout(int id, int key) {
    if (comp[id].Hout[key] == 0) {
        comp[id].Hout[key] = 1;
        comp[id].HoutCnt = comp[id].HoutCnt + 1;
    }
}

//
//  Rule ATTR
//  Component component_id  assigns to key the evaluated expression
//
void attr(int component_id, int key, int value, int keyinexpression) {
    __VERIFIER_assume(comp[component_id].HoutCnt == 0);
    __VERIFIER_assume(comp[component_id].HinCnt == 0);
    comp[component_id].I[key] = value;
    now(); // local step
    if (keyinexpression!=-1) {
        setHin(component_id, keyinexpression);
    }
}

//
//  Rule LSTIG
//
void lstig(int component_id, int key, int value, int keyinexpression) {
    __VERIFIER_assume(comp[component_id].HoutCnt == 0);
    __VERIFIER_assume(comp[component_id].HinCnt == 0);
    comp[component_id].Lvalue[key] = value;
    comp[component_id].Ltstamp[key] = now();

    if ((keyinexpression!=-1)) {
        setHin(component_id, keyinexpression);
    }

    setHout(component_id, key);
}

void confirm(void) {
    unsigned char guessedcomp;
    __VERIFIER_assume(guessedcomp < MAXPROCS);
    __VERIFIER_assume(comp[guessedcomp].HinCnt > 0);
    // __VERIFIER_assume(comp[guessedcomp].HoutCnt == 0); // Priority to propagate()

    unsigned char guessedkey;
    __VERIFIER_assume(guessedkey < MAXKEY);
    __VERIFIER_assume(comp[guessedcomp].Hin[guessedkey] == 1);

    int i;
    int t = comp[guessedcomp].Ltstamp[guessedkey];
    ////printf(">>>[%d] start Hin (%d)\n", guessedcomp, guessedkey);    
    
    for (i=0; i<MAXPROCS; i++) {
        if ( (guessedcomp!=i) && link(guessedcomp,i) && (differentLstig(guessedcomp, i, guessedkey)) ) {
            if (comp[i].Ltstamp[guessedkey]<=t) {
                comp[i].Lvalue[guessedkey] = comp[guessedcomp].Lvalue[guessedkey];
                comp[i].Ltstamp[guessedkey] = t;
                setHout(i, guessedkey);
            }
            else { //(comp[i].Ltstamp[guessedkey]>t)
                setHout(i, guessedkey);
            }
        }
    }

    comp[guessedcomp].Hin[guessedkey] = 0;
    comp[guessedcomp].HinCnt--;
}

void propagate(void) {
    unsigned char guessedcomp;
     __VERIFIER_assume(guessedcomp < MAXPROCS);
     __VERIFIER_assume(comp[guessedcomp].HoutCnt > 0);
     // __VERIFIER_assume(comp[guessedcomp].HinCnt == 0); // Priority to Confirm()

    unsigned char guessedkey;
    __VERIFIER_assume(guessedkey < MAXKEY);
    __VERIFIER_assume(comp[guessedcomp].Hout[guessedkey] == 1);

    int i;
    int t = comp[guessedcomp].Ltstamp[guessedkey];

    for (i=0; i<MAXPROCS; i++) {

        if ((guessedcomp!=i) && (link(guessedcomp,i)) && (comp[i].Ltstamp[guessedkey]<t)) {
            comp[i].Lvalue[guessedkey] = comp[guessedcomp].Lvalue[guessedkey];
            comp[i].Ltstamp[guessedkey] = t;
            setHout(i, guessedkey);
        }
    }

    comp[guessedcomp].Hout[guessedkey] = 0;
    comp[guessedcomp].HoutCnt = comp[guessedcomp].HoutCnt - 1;
}
"""