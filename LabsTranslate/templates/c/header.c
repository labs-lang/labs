{%-if simulation-%}
_Bool __sim_spurious = 0;
char* format;
#define __sim_assert(COND, LBL) if (!(COND)) format = ("(SIMULATION) Violation: " LBL)
#define __sim_satisfied(COND, LBL) if (COND) format = ("(SIMULATION) Satisfied: " LBL)

{%-endif-%}
const char undef_value = -128;
const _Bool SIMULATION = {%if simulation%}1{%else%}0{%endif%};
const {{typeofBOUND}} BOUND = {{ bound }};
const {{ typeofMAXCOMPONENTS }} MAXCOMPONENTS = {{ MAXCOMPONENTS }};
const {{ typeofMAXPC }} MAXPC = {{ MAXPC }};
{%- if bound > 0 -%}
unsigned __LABS_step = 0;
{%- endif -%}

{% for item in typedefs -%}
typedef {{item.value}} {{item.name}};
{% endfor %}

TYPEOFVALUES __abs(TYPEOFVALUES x) {
  return (x>0) ? x : -x;
}

TYPEOFVALUES __max(TYPEOFVALUES x, TYPEOFVALUES y) { return (x > y) ? x : y; }
TYPEOFVALUES __min(TYPEOFVALUES x, TYPEOFVALUES y) { return (x < y) ? x : y; }
TYPEOFVALUES __round_div(TYPEOFVALUES num, TYPEOFVALUES den) {
    return (num*den>=0) ? (num + (den/2)) / den : -((-num + (den/2)) / den) ;
}

TYPEOFVALUES mod(TYPEOFVALUES n, TYPEOFVALUES m) {
  return n >= 0 ? n % m : m + (n % m);
}

TYPEOFVALUES nondetInRange(TYPEOFVALUES minValue, TYPEOFVALUES bound) {
  TYPEOFVALUES x;
  __CPROVER_assume((x >= minValue) & (x < bound));
  return x;

}

const TYPEOFKEYIID MAXKEYI = {{ MAXKEYI }};
TYPEOFVALUES I[{{ MAXCOMPONENTS }}][{{ MAXKEYI }}];

TYPEOFPC pc[{{ MAXCOMPONENTS }}][{{ MAXPC }}];

{%- if hasStigmergy -%}
const TYPEOFKEYLID MAXKEYL = {{ MAXKEYL }};
TYPEOFTIME __LABS_time;
_Bool Hin[{{ MAXCOMPONENTS }}][{{ MAXKEYL }}];
_Bool Hout[{{ MAXCOMPONENTS }}][{{ MAXKEYL }}]; 
unsigned char HinCnt[{{ MAXCOMPONENTS }}];
unsigned char HoutCnt[{{ MAXCOMPONENTS }}];

TYPEOFTIME now(void) {
    return ++__LABS_time;
}

TYPEOFVALUES Lvalue[{{ MAXCOMPONENTS }}][{{ MAXKEYL }}];
TYPEOFTIME Ltstamp[{{ MAXCOMPONENTS }}][{{ MAXKEYL }}];

const {{ typeofMAXTUPLE }} MAXTUPLE = {{ MAXTUPLE }};
const TYPEOFKEYLID tupleStart[{{ MAXKEYL }}] = { {{ tupleStart | join: ", " }} };
const TYPEOFKEYLID tupleEnd[{{ MAXKEYL }}] = { {{ tupleEnd | join: ", " }} };

_Bool link(TYPEOFAGENTID __LABS_link1, TYPEOFAGENTID __LABS_link2, TYPEOFKEYLID key) {
    _Bool __LABS_link = 0;
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
    if (!Hin[id][tupleStart[key]]) HinCnt[id] = HinCnt[id] + 1;  
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
    if (!Hout[id][tupleStart[key]]) HoutCnt[id] = HoutCnt[id] + 1;
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
{%- endif -%}

//
//  Rule ATTR
//  Component component_id  assigns to key the evaluated expression
//  If check is true, transition is guarded by HoutCnt == HinCnt == 0
//
void attr(TYPEOFAGENTID id, TYPEOFKEYIID key, TYPEOFVALUES value, _Bool check) {
    {%- if hasStigmergy -%}
    __CPROVER_assume((!check) | (HoutCnt[id] == 0));
    __CPROVER_assume((!check) | (HinCnt[id] == 0));
    {%- endif -%}


    I[id][key] = value;
    {%- if hasStigmergy -%}
    now(); // local step
    {%- endif -%}
}

{%- if hasEnvironment -%}
const TYPEOFKEYEID MAXKEYE = {{ MAXKEYE }};
TYPEOFVALUES E[{{ MAXKEYE }}];
void env(TYPEOFAGENTID id, TYPEOFKEYEID key, TYPEOFVALUES value, _Bool check) {
    {%- if hasStigmergy -%}
    __CPROVER_assume((!check) | (HoutCnt[id] == 0));
    __CPROVER_assume((!check) | (HinCnt[id] == 0));
    {%- endif -%}

    E[key] = value;
    {%- if hasStigmergy -%}
    now(); // local step
    {%- endif -%}
}
{%- endif -%}

// ___concrete-globals___
// ___end concrete-globals___

{%- if hasStigmergy -%}
//
//  Rule LSTIG
//
void lstig(TYPEOFAGENTID id, TYPEOFKEYLID key, TYPEOFVALUES value, _Bool check) {
    __CPROVER_assume((!check) | (HoutCnt[id] == 0));
    __CPROVER_assume((!check) | (HinCnt[id] == 0));

    Lvalue[id][key] = value;
    // Only update the timestamp of the 1st element in the tuple
    Ltstamp[id][tupleStart[key]] = now();
    
    setHout(id, key);
}

_Bool differentLstig(TYPEOFAGENTID comp1, TYPEOFAGENTID comp2, TYPEOFKEYLID key) {
    TYPEOFKEYLID k  = tupleStart[key];
    return ((Lvalue[comp1][k] != Lvalue[comp1][k]) | (Ltstamp[comp1][k] != Ltstamp[comp2][k]));
}

void confirm(void) {
    TYPEOFAGENTID guessedcomp;
    __CPROVER_assume(guessedcomp < MAXCOMPONENTS);
    __CPROVER_assume(HinCnt[guessedcomp] > 0);

    TYPEOFKEYLID guessedkey;
    __CPROVER_assume(guessedkey < MAXKEYL);
    __CPROVER_assume(Hin[guessedcomp][guessedkey] == 1);

    // NOTE: Since SetHin(), SetHout() only work on tupleStarts,
    // guessedkey is guaranteed to be the 1st element of some tuple

    // assert(guessedkey == tupleStart[guessedkey]);

    TYPEOFAGENTID i;
    TYPEOFTIME t = timeof(guessedcomp, guessedkey);
    
    // Send data from guessedcomp to i
    for (i=0; i<MAXCOMPONENTS; i++) {
        if (((guessedcomp!=i) & (timeof(i, guessedkey) != t)) & link(guessedcomp,i,guessedkey)) {
            
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
    __CPROVER_assume(guessedcomp < MAXCOMPONENTS);
    __CPROVER_assume(HoutCnt[guessedcomp] > 0);

    TYPEOFKEYLID guessedkey;
    __CPROVER_assume(guessedkey < MAXKEYL);
    __CPROVER_assume(Hout[guessedcomp][guessedkey] == 1);

    // assert(guessedkey == tupleStart[guessedkey]);

    TYPEOFAGENTID i;
    TYPEOFTIME t = timeof(guessedcomp, guessedkey);

    for (i=0; i<MAXCOMPONENTS; i++) {
        if (((guessedcomp!=i) & (timeof(i, guessedkey)<t)) & (link(guessedcomp,i,guessedkey))) {
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
{%- endif -%}

// ___includes___
// ___end includes___
