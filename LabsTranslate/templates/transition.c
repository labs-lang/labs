void {{label}}(int tid) {
    // {{labs}}

    {%- for guard in guards -%}{{guard}}
    {%- endfor -%}
    {%- for item in entrypoints -%}
    __VERIFIER_assume(pc[tid][{{item.pc}}] == {{item.value}});
    {%- endfor -%}

    int val = {{expr}};
    int offset = {{offset}};
    {%- if size != 0 -%}
    assert(offset >= 0 && offset < size);
    {%- endif -%}

    {%- if type == "attr" -%}
    attr(tid, {{key}} + offset, val);
    {%- elsif type == "lstig" -%}
    lstig(tid, {{key}} + offset, val);
    {%- elsif type == "env" -%}
    env(tid, {{key}} + offset, val);
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
