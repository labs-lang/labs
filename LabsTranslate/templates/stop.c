void {{label}}(int tid) {

    {%- for guard in guards -%}
    {{guard}}
    {%- endfor -%}

    {%- for item in entrypoints -%}
    __VERIFIER_assume(pc[tid][{{item.pc}}] == {{item.value}});
    {%- endfor -%}

    term[tid] = 1;
    pc[tid][{{exitpc}}] = {{exitvalue}};
    
}
