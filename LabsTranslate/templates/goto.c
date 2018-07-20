void {{label}}(int tid) {
    {%- for guard in guards -%}{{guard}}
    {%- endfor -%}
    {%- for item in entrypoints -%}
    __VERIFIER_assume(pc[tid][{{item.pc}}] == {{item.value}});
    {%- endfor -%}
    int i;
    for (i=1; i<MAXPC; i++) {
        pc[tid][i] = 0;
    }

    pc[tid][{{exitpc}}] = {{exitvalue}};
}
