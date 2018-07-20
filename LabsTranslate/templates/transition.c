void {{label}}(int tid) {
    // {{labs}}

    {%- for guard in guards -%}{{guard}}
    {%- endfor -%}
    {%- for item in entrypoints -%}
    __VERIFIER_assume(pc[tid][{{item.pc}}] == {{item.value}});
    {%- endfor -%}

    int val = {{expr}};
    {%- if type == "attr" -%}
    attr(tid, {{key}}, val);
    {%- elsif type == "lstig" -%}
    lstig(tid, {{key}}, val);
    {%- elsif type == "env" -%}
    env({{key}}, val);
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
