void init() {
    TYPEOFVALUES _I[MAXCOMPONENTS][MAXKEYI];
    TYPEOFPC _pc[MAXCOMPONENTS][MAXPC];
    {%- if hasEnvironment -%}
    TYPEOFVALUES _E[MAXKEYE];
    {%- endif -%}
    {%- if hasStigmergy -%}
    TYPEOFVALUES _Lvalue[MAXCOMPONENTS][MAXKEYL];
    {%- endif -%}

    unsigned char i, j;
    {%- if hasStigmergy -%}
    for (i=0; i<MAXCOMPONENTS; i++) {
        for (j=0; j<MAXKEYL; j++) {
            Ltstamp[i][j] = 0;
            Hin[i][j] = 0;
            Hout[i][j] = 0;
        }
        HinCnt[i] = 0;
        HoutCnt[i] = 0;
    }
    {%- endif -%}

    {%- for agent in agents -%}
    {%- assign a = agent.end | minus: 1 -%}
    {%- for i in (agent.start..a) -%}

    {%- for p in agent.pcs -%}
    {%- if p.value.size == 1 -%}
    _pc[{{i}}][{{ p.name }}] = {{ p.value.first }};
    {%- else -%}
    __CPROVER_assume({%- for val in p.value -%} (_pc[{{i}}][{{ p.name }}] == {{ val }}){% unless forloop.last %} | {% endunless %}{%- endfor-%});
    {%- endif -%}{%- endfor -%}{%- endfor -%}{%- endfor -%}
        
    {%- for item in initenv -%}
    __CPROVER_assume({{ item.bexpr }});
    {%- endfor -%}
    {%- for agent in agents -%}
    {%- for item in agent.initvars -%}
    __CPROVER_assume({{ item.bexpr }});
    {%- endfor -%}
    {%- endfor -%}
    {%- if hasStigmergy -%}
    {%- for item in tstamps -%}
    Ltstamp[{{item.tid}}][tupleStart[{{item.index}}]] = now();
    {%- endfor -%}
    now();
    {%- endif -%}

    {%- if hasEnvironment -%}
    for (i=0; i<MAXKEYE; i++) {
        E[i] = _E[i];
    }
    {%- endif -%}
    for (i=0; i<MAXCOMPONENTS; i++) {
        for (j=0; j<MAXPC; j++) {
            pc[i][j] = _pc[i][j];
        }

        for (j=0; j<MAXKEYI; j++) {
            I[i][j] = _I[i][j];
        }
        {%- if hasStigmergy -%}
        for (j=0; j<MAXKEYL; j++) {
            Lvalue[i][j] = _Lvalue[i][j];
        }
        {%- endif -%}
    }
}
