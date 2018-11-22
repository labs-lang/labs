{% for item in defines -%}
#define {{item.name}} {{item.value}}
{% endfor -%}
#define undef_value -128 // SHRT_MIN

#define LABSassume(COND)            __VERIFIER_assume(COND)   

#ifdef SIMULATION
    #define LABScheck(pcs, guards)      ((pcs) & (guards))
    #define LABSassert(COND, LABEL)     if(!(COND)) { printf(">>>" #LABEL " violated"); } else { printf(">>>" #LABEL " satisfied"); } 
#else 
    #define LABScheck(pcs, guards)      (pcs)
    #define LABSassert(COND, LABEL)     /*#LABEL*/ assert(COND)
#endif


{% for item in typedefs -%}
typedef {{item.value}} {{item.name}};
{% endfor %}

typedef unsigned __CPROVER_bitvector[1] Bool;


TYPEOFVALUES abs(TYPEOFVALUES x) {
  return (x>0) ? x : -x;
}

TYPEOFVALUES mod(TYPEOFVALUES n, TYPEOFVALUES m) {
  return n >= 0 ? n % m : m + (n % m);
}

TYPEOFVALUES I[MAXCOMPONENTS][MAXKEYI];
TYPEOFVALUES Lvalue[MAXCOMPONENTS][MAXKEYL];
TYPEOFTIME Ltstamp[MAXCOMPONENTS][MAXKEYL];
TYPEOFVALUES E[MAXKEYE];

Bool Hin[MAXCOMPONENTS][MAXKEYL];
Bool Hout[MAXCOMPONENTS][MAXKEYL]; 
unsigned char HinCnt[MAXCOMPONENTS];
unsigned char HoutCnt[MAXCOMPONENTS];
Bool terminated[MAXCOMPONENTS];
unsigned char pc[MAXCOMPONENTS][MAXPC];
TYPEOFTIME __LABS_time;

const unsigned char tupleStart[MAXKEYL] = { {{ tupleStart | join: ", " }} };
const unsigned char tupleEnd[MAXKEYL] = { {{ tupleEnd | join: ", " }} };


Bool link(TYPEOFAGENTID __LABS_link1, TYPEOFAGENTID __LABS_link2, TYPEOFKEYLID key) {
    Bool __LABS_link = 0;
    {%- for l in links -%}
        {%- if forloop.first -%}
    if ((key >= {{l.start}}) & (key <= {{l.end}})){
        {%- else -%}
    else if ((key >= {{l.start}}) & (key <= {{l.end}})){
        {%- endif -%}
        __LABS_link = {{l.link}};
    }
    {%- endfor -%}

    return __LABS_link;
}

TYPEOFTIME now(void) {
    return ++__LABS_time;
}

void setHin(TYPEOFAGENTID id, TYPEOFKEYLID key) {
    if (Hin[id][tupleStart[key]] == 0) {
        Hin[id][tupleStart[key]] = 1;
        HinCnt[id] = HinCnt[id] + 1;
    }
}

void clearHin(TYPEOFAGENTID id, TYPEOFKEYLID key) {
    if (Hin[id][tupleStart[key]] == 1) {
        Hin[id][tupleStart[key]] = 0;
        HinCnt[id] = HinCnt[id] - 1;
    }
}

void setHout(TYPEOFAGENTID id, TYPEOFKEYLID key) {
    if (Hout[id][tupleStart[key]] == 0) {
        Hout[id][tupleStart[key]] = 1;
        HoutCnt[id] = HoutCnt[id] + 1;
    }
}

void clearHout(TYPEOFAGENTID id, TYPEOFKEYLID key) {
    if (Hout[id][tupleStart[key]] == 1) {
        Hout[id][tupleStart[key]] = 0;
        HoutCnt[id] = HoutCnt[id] - 1;
    }
}

//
//  Rule ATTR
//  Component component_id  assigns to key the evaluated expression
//  If check is true, transition is guarded by HoutCnt == HinCnt == 0
//
void attr(TYPEOFAGENTID id, TYPEOFKEYIID key, TYPEOFVALUES value, Bool check) {
    __VERIFIER_assume((!check) | (HoutCnt[id] == 0));
    __VERIFIER_assume((!check) | (HinCnt[id] == 0));

    I[id][key] = value;
    now(); // local step
}

//
//  Rule LSTIG
//
void lstig(TYPEOFAGENTID id, TYPEOFKEYLID key, TYPEOFVALUES value, Bool check) {
    __VERIFIER_assume((!check) | (HoutCnt[id] == 0));
    __VERIFIER_assume((!check) | (HinCnt[id] == 0));

    Lvalue[id][key] = value;
    // Only update the timestamp of the 1st element in the tuple
    Ltstamp[id][tupleStart[key]] = now();
    
    setHout(id, key);
}

void env(TYPEOFAGENTID id, TYPEOFKEYEID key, TYPEOFVALUES value, Bool check) {
    __VERIFIER_assume((!check) | (HoutCnt[id] == 0));
    __VERIFIER_assume((!check) | (HinCnt[id] == 0));
    
    E[key] = value;
    now(); // local step
}

Bool differentLstig(TYPEOFAGENTID comp1, TYPEOFAGENTID comp2, TYPEOFKEYLID key) {
    TYPEOFKEYLID k  = tupleStart[key];
    return ((Lvalue[comp1][k] != Lvalue[comp1][k]) | (Ltstamp[comp1][k] != Ltstamp[comp2][k]));
}

void confirm(void) {
    TYPEOFAGENTID guessedcomp;
    __VERIFIER_assume(guessedcomp < MAXCOMPONENTS);
    __VERIFIER_assume(HinCnt[guessedcomp] > 0);

    TYPEOFKEYLID guessedkey;
    __VERIFIER_assume(guessedkey < MAXKEYL);
    __VERIFIER_assume(Hin[guessedcomp][guessedkey] == 1);

    TYPEOFAGENTID i;
    TYPEOFKEYLID k;
    TYPEOFTIME t = Ltstamp[guessedcomp][guessedkey];
    
    for (i=0; i<MAXCOMPONENTS; i++) {
        if ( (guessedcomp!=i) && link(guessedcomp,i,guessedkey) && differentLstig(guessedcomp, i, guessedkey) ) {
            setHout(i, guessedkey);
            for (k = 0; k < MAXKEYL; k++) {
                if ((k >= tupleStart[guessedkey]) & (k <= tupleEnd[guessedkey])) {
                    if (Ltstamp[i][k]<=t) {
                        Lvalue[i][k] = Lvalue[guessedcomp][k];
                        Ltstamp[i][k] = t;
                    }
                }
            }
        }
    }
    clearHin(guessedcomp, guessedkey);
}

void propagate(void) {
    TYPEOFAGENTID guessedcomp;
     __VERIFIER_assume(guessedcomp < MAXCOMPONENTS);
     __VERIFIER_assume(HoutCnt[guessedcomp] > 0);

    TYPEOFKEYLID guessedkey;
    __VERIFIER_assume(guessedkey < MAXKEYL);
    __VERIFIER_assume(Hout[guessedcomp][guessedkey] == 1);

    TYPEOFAGENTID i;
    TYPEOFKEYLID k;
    TYPEOFTIME t = Ltstamp[guessedcomp][guessedkey];

    for (i=0; i<MAXCOMPONENTS; i++) {
        if ((guessedcomp!=i) && (Ltstamp[i][guessedkey]<t) && (link(guessedcomp,i,guessedkey))) {
            for (k = 0; k < MAXKEYL; k++) {
                if ((k >= tupleStart[guessedkey]) & (k <= tupleEnd[guessedkey])) {
                    Lvalue[i][k] = Lvalue[guessedcomp][k];
                    Ltstamp[i][k] = t;
                }
            }
            setHout(i, guessedkey);
        }
    }

    clearHout(guessedcomp, guessedkey);
}
