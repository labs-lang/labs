void init() {

    short _I[MAXCOMPONENTS][MAXKEYI];
    short _Lvalue[MAXCOMPONENTS][MAXKEYL];
    short _E[MAXKEYE];

    int i,j;
    for (i=0; i<MAXCOMPONENTS; i++) {
        terminated[i] = 0;
        for (j=0; j<MAXKEYL; j++) {
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

    for (i=0; i<MAXKEYE; i++) {
        E[i] = _E[i];
    }
    for (i=0; i<MAXCOMPONENTS; i++) {
        for (j=0; j<MAXKEYI; j++) {
            I[i][j] = _I[i][j];
        }
        for (j=0; j<MAXKEYL; j++) {
            Lvalue[i][j] = _Lvalue[i][j];
            Ltstamp[i][j] = Ltstamp[i][tupleEnd[j]];
        }
    }
}