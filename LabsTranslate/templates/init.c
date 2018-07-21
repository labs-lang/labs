void init() {

{% for t in tuples -%}
    tupleStart[{{t.index}}] = {{t.start}};
    tupleEnd[{{t.index}}] = {{t.end}};
{% endfor -%}

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
    
{{- initenv -}}

{{- initvars -}}

    __LABS_t = j;

    {% comment %}
    We set all items in a tuple to the timestamp of the last one
    {% endcomment %}
    for (i=0; i<MAXCOMPONENTS; i++) {
        for (j=0; j<MAXKEYL; j++) {
            Ltstamp[i][j] = Ltstamp[i][tupleEnd[j]];
        }
    }
}