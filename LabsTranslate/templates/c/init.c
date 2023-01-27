void init(void) {
    {%- for item in initenv -%}
        {%- if item.bexpr contains "&" or item.bexpr contains "|" or item.bexpr contains "<" or item.bexpr contains "!" or item.bexpr contains ">" -%}
    E[{{item.index}}] = __CPROVER_nondet_int();
    __CPROVER_assume({{ item.bexpr }});
        {%- else -%}
    {{ item.bexpr | replace: "==", "=" }};
        {%- endif -%}
    {%- endfor -%}

    unsigned char j = 0;
    {%- for agent in agents -%}
    {%- assign a = agent.end | minus: 1 -%}
    {%- for i in (agent.start..a) -%}

    // ___symbolic-init___
    for (j=0; j<MAXKEYI; j++) {
        I[{{i}}][j] = __CPROVER_nondet_int();
    }
    {%- if hasStigmergy -%}
    for (j=0; j<MAXKEYL; j++) {
        Lvalue[{{i}}][j] = __CPROVER_nondet_int();
        Ltstamp[{{i}}][j] = 0;
        Hin[{{i}}][j] = 0;
        Hout[{{i}}][j] = 0;
    }
    HinCnt[{{i}}] = 0;
    HoutCnt[{{i}}] = 0;
    {%- endif -%}
    // ___end symbolic-init___

    {%- for p in agent.pcs -%}
    {%- if p.value.size == 1 -%}
    pc[{{i}}][{{ p.name }}] = {{ p.value.first }};
    {%- else -%}
    pc[{{i}}][{{ p.name }}] = __CPROVER_nondet_int();
    __CPROVER_assume({%- for val in p.value -%} (pc[{{i}}][{{ p.name }}] == {{ val }}){% unless forloop.last %} | {% endunless %}{%- endfor-%});
    {%- endif -%}{%- endfor -%}{%- endfor -%}{%- endfor -%}

    // ___concrete-init___
    // ___end concrete-init___
    
    // ___symbolic-init___
    {%- for agent in agents -%}
    {%- for item in agent.initvars -%}
        {%- if item.bexpr contains "&" or item.bexpr contains "|" or item.bexpr contains "<" or item.bexpr contains "!" or item.bexpr contains ">" -%}
    __CPROVER_assume({{ item.bexpr }});
        {%- else -%}
    {{ item.bexpr | replace: "==", "=" }};
        {%- endif -%}
    {%- endfor -%}
    {%- endfor -%}

    {%-for item in assumes-%}
    __CPROVER_assume({{item.value}}); //{{item.name}}
    {%-endfor-%}
    // ___end symbolic-init___
    

    {%- if hasStigmergy -%}
    {%- for item in tstamps -%}
    Ltstamp[{{item.tid}}][tupleStart[{{item.index}}]] = now();
    {%- endfor -%}
    now();
    {%- endif -%}

}
