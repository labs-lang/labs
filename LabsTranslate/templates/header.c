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

TYPEOFVALUES __abs(TYPEOFVALUES x) {
  return (x>0) ? x : -x;
}

TYPEOFVALUES __max(TYPEOFVALUES x, TYPEOFVALUES y) { return (x > y) ? x : y; }
TYPEOFVALUES __min(TYPEOFVALUES x, TYPEOFVALUES y) { return (x < y) ? x : y; }

TYPEOFVALUES mod(TYPEOFVALUES n, TYPEOFVALUES m) {
  return n >= 0 ? n % m : m + (n % m);
}

TYPEOFVALUES I[MAXCOMPONENTS][MAXKEYI];
TYPEOFVALUES E[MAXKEYE];
Bool terminated[MAXCOMPONENTS];

TYPEOFPC pc[MAXCOMPONENTS][MAXPC];

#if DISABLELSTIG == 0

TYPEOFTIME __LABS_time;
Bool Hin[MAXCOMPONENTS][MAXKEYL];
Bool Hout[MAXCOMPONENTS][MAXKEYL]; 
unsigned char HinCnt[MAXCOMPONENTS];
unsigned char HoutCnt[MAXCOMPONENTS];

TYPEOFTIME now(void) {
    return ++__LABS_time;
}

TYPEOFVALUES Lvalue[MAXCOMPONENTS][MAXKEYL];
TYPEOFTIME Ltstamp[MAXCOMPONENTS][MAXKEYL];

const TYPEOFKEYLID tupleStart[MAXKEYL] = { {{ tupleStart | join: ", " }} };
const TYPEOFKEYLID tupleEnd[MAXKEYL] = { {{ tupleEnd | join: ", " }} };

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

TYPEOFTIME timeof(TYPEOFAGENTID id, TYPEOFKEYLID key) {
    return Ltstamp[id][tupleStart[key]];
}

void setHin(TYPEOFAGENTID id, TYPEOFKEYLID key) {
    // if (Hin[id][tupleStart[key]] == 0) {
    //     Hin[id][tupleStart[key]] = 1;
    //     HinCnt[id] = HinCnt[id] + 1;
    // }
    if (!Hin[id][tupleStart[key]]) ++HinCnt[id];
    Hin[id][tupleStart[key]] = 1;
}

void clearHin(TYPEOFAGENTID id, TYPEOFKEYLID key) {
    // if (Hin[id][tupleStart[key]] == 1) {
    //     Hin[id][tupleStart[key]] = 0;
    //     HinCnt[id] = HinCnt[id] - 1;
    // }
    HinCnt[id] = HinCnt[id] - (Hin[id][tupleStart[key]]);
    // assert(HinCnt[id] >= 0);
    Hin[id][tupleStart[key]] = 0;
}

void setHout(TYPEOFAGENTID id, TYPEOFKEYLID key) {
    // if (Hout[id][tupleStart[key]] == 0) {
    //     Hout[id][tupleStart[key]] = 1;
    //     HoutCnt[id] = HoutCnt[id] + 1;
    // }
    if (!Hout[id][tupleStart[key]]) ++HoutCnt[id];
    Hout[id][tupleStart[key]] = 1;
}

void clearHout(TYPEOFAGENTID id, TYPEOFKEYLID key) {
    // if (Hout[id][tupleStart[key]] == 1) {
    //     Hout[id][tupleStart[key]] = 0;
    //     HoutCnt[id] = HoutCnt[id] - 1;
    // }
    // assert(HoutCnt[id] > 0);
    HoutCnt[id] = HoutCnt[id] - (Hout[id][tupleStart[key]]);
    Hout[id][tupleStart[key]] = 0;
}
#endif

//
//  Rule ATTR
//  Component component_id  assigns to key the evaluated expression
//  If check is true, transition is guarded by HoutCnt == HinCnt == 0
//
void attr(TYPEOFAGENTID id, TYPEOFKEYIID key, TYPEOFVALUES value, Bool check) {
    #if DISABLELSTIG == 0
    __VERIFIER_assume((!check) | (HoutCnt[id] == 0));
    __VERIFIER_assume((!check) | (HinCnt[id] == 0));
    #endif

    I[id][key] = value;
    #if DISABLELSTIG == 0
    now(); // local step
    #endif
}

void env(TYPEOFAGENTID id, TYPEOFKEYEID key, TYPEOFVALUES value, Bool check) {
    #if DISABLELSTIG == 0
    __VERIFIER_assume((!check) | (HoutCnt[id] == 0));
    __VERIFIER_assume((!check) | (HinCnt[id] == 0));
    #endif

    E[key] = value;
    #if DISABLELSTIG == 0
    now(); // local step
    #endif
}

#if DISABLELSTIG == 0
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

    // NOTE: Since SetHin(), SetHout() only work on tupleStarts,
    // guessedkey is guaranteed to be the 1st element of some tuple

    // assert(guessedkey == tupleStart[guessedkey]);

    TYPEOFAGENTID i;
    TYPEOFTIME t = timeof(guessedcomp, guessedkey);
    
    // Send data from guessedcomp to i
    for (i=0; i<MAXCOMPONENTS; i++) {
        if (((guessedcomp!=i) & (timeof(i, guessedkey) != t)) && 
            link(guessedcomp,i,guessedkey)) {
            
            setHout(i, guessedkey);
            // If data is fresh, agent i copies it to its stigmergy
            if (timeof(i, guessedkey) < t) {
                TYPEOFKEYLID k, next;
                clearHin(i, guessedkey);
                for (k = 0; k < MAXTUPLE; k++) {
                    next = guessedkey + k;
                    // if ((next<MAXKEYL) && (tupleStart[next] == guessedkey))
                    if (next <= tupleEnd[guessedkey])
                        Lvalue[i][next] = Lvalue[guessedcomp][next];
                }
                Ltstamp[i][guessedkey] = t;
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

    // assert(guessedkey == tupleStart[guessedkey]);

    TYPEOFAGENTID i;
    TYPEOFTIME t = timeof(guessedcomp, guessedkey);

    for (i=0; i<MAXCOMPONENTS; i++) {
        if (((guessedcomp!=i) & (timeof(i, guessedkey)<t)) && (link(guessedcomp,i,guessedkey))) {
            // If data is fresh, i copies it to its stigmergy and
            // will propagate it in the future (setHout)
            setHout(i, guessedkey);
            clearHin(i, guessedkey);
            TYPEOFKEYLID k, next;
            for (k = 0; k < MAXTUPLE; k++) {
                next = guessedkey+k;
                // if (next<MAXKEYL && tupleStart[next] == tupleStart[guessedkey])
                if (next <= tupleEnd[guessedkey])
                    Lvalue[i][next] = Lvalue[guessedcomp][next];
            }
            Ltstamp[i][guessedkey] = t;

        }
    }
    clearHout(guessedcomp, guessedkey);
}
#endif
