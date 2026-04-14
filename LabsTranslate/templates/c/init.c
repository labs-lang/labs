void init(void) {
    
    {%-assign maxi = MAXKEYI | minus: 1-%}
    {%-assign maxe = MAXKEYE | minus: 1-%}
    {%-if hasEnvironment-%}
    {%- for i in (0..maxe) -%}
    E[{{ i }}] = nondetInit();
    {%- endfor -%}
    {%- endif -%}
    {%-if hasStigmergy-%}unsigned char j = 0;{%-endif-%}
    {%- for agent in agents -%}
    {%- assign a = agent.end | minus: 1 -%}
    {%- for i in (agent.start..a) -%}
    {%- if hasStigmergy -%}
    for (j=0; j<MAXKEYL; j++) {
        Lvalue[[{{i}}]][j] = nondetInit();
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
    pc[{{i}}][{{ p.name }}] = nondetInit();
    {{cAssume}}({%- for val in p.value -%} (pc[{{i}}][{{ p.name }}] == {{ val }}){% unless forloop.last %} {{cOr}} {% endunless %}{%- endfor-%});
    {%- endif -%}{%- endfor -%}{%- endfor -%}{%- endfor -%}

    // ___concrete-init___
    // ___end concrete-init___
    
    // ___symbolic-init___
    {%- for agent in agents -%}
    {%-assign end = agent.end | minus: 1-%}
    {%- for i in (agent.start..end) -%}
    {%- for j in (0..maxi) -%}
    I[{{ i }}][{{ j }}] = nondetInit();
    {%- endfor -%}
    {%- endfor -%}
    {%- endfor -%}

    {%-for item in assumes-%}
    {{cAssume}}({{item.value}}); //{{item.name}}
    {%-endfor-%}
    // ___end symbolic-init___
    

    {%- if hasStigmergy -%}
    {%- for item in tstamps -%}
    Ltstamp[{{item.tid}}][tupleStart[{{item.index}}]] = now();
    {%- endfor -%}
    now();
    {%- endif -%}

}
