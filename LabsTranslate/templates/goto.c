void {{label}}(int tid) {
    {%- for item in entrypoints -%}
    LABSassume(pc[tid][{{item.pc}}] == {{item.value}});{%- endfor -%}
    {%- for guard in guards -%}
    LABSassume({{guard}});{%- endfor -%}

    {%- if resetpcs -%}
    int i;
    for (i=1; i<MAXPC; i++) {
        pc[tid][i] = 0;
    }{%- endif -%}

    pc[tid][{{exitpc}}] = {{exitvalue}};
}
