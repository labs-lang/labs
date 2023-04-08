void init(void) {
    {%- for item in initenv -%}
        {%- if item.bexpr contains "&" or item.bexpr contains "|" or item.bexpr contains "<" or item.bexpr contains "!" or item.bexpr contains ">" -%}
    E[{{item.index}}] = __CPROVER_nondet();
    __CPROVER_assume({{ item.bexpr }});
        {%- elsif item.bexpr contains "== (0)" -%}
        {%- else -%}
    {{ item.bexpr | replace: "==", "=" }};
        {%- endif -%}
    {%- endfor -%}
    {%-if hasStigmergy-%}unsigned char j = 0;{%-endif-%}
    {%- for agent in agents -%}
    {%- assign a = agent.end | minus: 1 -%}
    {%- for i in (agent.start..a) -%}
    {%- if hasStigmergy -%}
    for (j=0; j<MAXKEYL; j++) {
        Ltstamp[{{i}}][j] = 0;
        Hin[{{i}}][j] = 0;
        Hout[{{i}}][j] = 0;
    }
    HinCnt[{{i}}] = 0;
    HoutCnt[{{i}}] = 0;
    {%- endif -%}

    {%- for p in agent.pcs -%}
    {%- if p.value.size == 1 -%}
    pc[{{i}}][{{ p.name }}] = {{ p.value.first }};
    {%- else -%}
    pc[{{i}}][{{ p.name }}] = __CPROVER_nondet();
    __CPROVER_assume({%- for val in p.value -%} (pc[{{i}}][{{ p.name }}] == {{ val }}){% unless forloop.last %} | {% endunless %}{%- endfor-%});
    {%- endif -%}{%- endfor -%}{%- endfor -%}{%- endfor -%}

    // ___concrete-init___
    // ___end concrete-init___
    
    // ___symbolic-init___
    {%- for agent in agents -%}
    {%- for item in agent.initvars -%}
        {%- if item.bexpr contains "&" or item.bexpr contains "|" or item.bexpr contains "<" or item.bexpr contains "!" or item.bexpr contains ">" -%}
    {%-assign tid = item.bexpr | split: "["-%}
    {%if item.loc == "L" %}Lvalue{%else%}I{%endif%}[{{ tid[1] | remove: "]" }}][{{item.index}}] = __CPROVER_nondet();
    __CPROVER_assume({{ item.bexpr }});
        {%- elsif item.bexpr contains "== (0)" -%}
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
