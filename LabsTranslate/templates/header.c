{% for item in defines -%}
#define {{item.name}} {{item.value}}
{% endfor -%}
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

int link(int __LABS_link1, int __LABS_link2) {
    int __LABS_link = {{link}};
    return __LABS_link;
}

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