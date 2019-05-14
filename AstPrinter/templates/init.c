void init() {

    TYPEOFVALUES _I[MAXCOMPONENTS][MAXKEYI];
    TYPEOFVALUES _Lvalue[MAXCOMPONENTS][MAXKEYL];
    TYPEOFVALUES _E[MAXKEYE];
    TYPEOFPC _pc[MAXCOMPONENTS][MAXPC];

    unsigned char i, j;
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
    {%- assign a = item.end | minus: 1 -%}
    {%- for i in (item.start..a) -%}

    {%- for p in item.pcs -%}
    {%- if p.value.size == 1 -%}
    _pc[{{i}}][{{ p.name }}] = {{ p.value.first }};
    {%- else -%}
    LABSassume({%- for val in p.value -%} (_pc[{{i}}][{{ p.name }}] == {{ val }}){% unless forloop.last %} | {% endunless %}{%- endfor-%});
    {%- endif -%}{%- endfor -%}{%- endfor -%}{%- endfor -%}
        
    {%- for item in initenv -%}
    LABSassume({{item}});
    {%- endfor -%}
    {%- for item in initvars -%}
    LABSassume({{item}});
    {%- endfor -%}
    {%- for item in tstamps -%}
    Ltstamp[{{item.tid}}][tupleStart[{{item.index}}]] = now();
    {%- endfor -%}
    now();

    for (i=0; i<MAXKEYE; i++) {
        E[i] = _E[i];
    }
    for (i=0; i<MAXCOMPONENTS; i++) {
        for (j=0; j<MAXPC; j++) {
            pc[i][j] = _pc[i][j];
        }

        for (j=0; j<MAXKEYI; j++) {
            I[i][j] = _I[i][j];
        }

        for (j=0; j<MAXKEYL; j++) {
            Lvalue[i][j] = _Lvalue[i][j];
        }
    }
}
