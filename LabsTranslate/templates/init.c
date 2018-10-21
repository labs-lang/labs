void init() {

{% for t in tuples -%}
    tupleStart[{{t.index}}] = {{t.start}};
    tupleEnd[{{t.index}}] = {{t.end}};
{% endfor -%}

int i,j;
for (i=0; i<MAXKEYE; i++) {
        E[i] = nondet();
    }
for (i=0; i<MAXCOMPONENTS; i++) {
    terminated[i] = 0;
    for (j=0; j<MAXKEYI; j++) {
        I[i][j] = nondet();
    }
    for (j=0; j<MAXKEYI; j++) {
        I[i][j] = nondet();
    }
    for (j=0; j<MAXKEYL; j++) {
        Lvalue[i][j] = nondet();
        Ltstamp[i][j] = 0;
        Hin[i][j] = 0;
        Hout[i][j] = 0;
    }
    HinCnt[i] = 0;
    HoutCnt[i] = 0;
}

    {%- for item in initpcs -%}
    for (i={{item.start}}; i<{{item.end}}; i++) {
        pc[i][0] == {{item.pc}};
    }
    {%- endfor -%}

    j=0;
        
    {{- initenv -}}

    {{- initvars -}}

    __LABS_time = j;

    {% comment %}
    We set all items in a tuple to the timestamp of the last one
    {% endcomment %}
    for (i=0; i<MAXCOMPONENTS; i++) {
        for (j=0; j<MAXKEYL; j++) {
            Ltstamp[i][j] = Ltstamp[i][tupleEnd[j]];
        }
    }
}