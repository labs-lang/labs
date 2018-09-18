void {{label}}(int tid) {
    //{{labs}}

    {%- for item in entrypoints -%}
    __VERIFIER_assume(pc[tid][{{item.pc}}] == {{item.value}});
    {%- endfor -%}
    {{guards}}

    int val = {{expr}};
    {%- if size != 0 -%}
    int offset = {{offset}};
    assert(offset >= 0 && offset < {{size}});
    {{type}}(tid, {{key}} + offset, val);
    {%- else -%}
    {{type}}(tid, {{key}}, val);
    {%- endif -%}

    {%- for k in qrykeys -%}
    setHin(tid, {{k}});
    {%- endfor -%}
    {%- if resetpcs -%}
    int i;
    for (i=1; i<MAXPC; i++) {
        pc[tid][i] = 0;
    }
    {%- endif -%}

    pc[tid][{{exitpc}}] = {{exitvalue}};  
}
