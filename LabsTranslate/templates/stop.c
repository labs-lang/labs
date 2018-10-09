void {{label}}(int tid) {

    {%- for item in entrypoints -%}
    LABSassume(pc[tid][{{item.pc}}] == {{item.value}});{%- endfor -%}
    {%- for guard in guards -%}
    LABSassume({{guard}});{%- endfor -%}

    term[tid] = 1;
    pc[tid][{{exitpc}}] = {{exitvalue}};
    
}
