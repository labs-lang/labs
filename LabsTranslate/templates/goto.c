void {{label}}(int tid) {
    {% include "templates/entry" with entrypoints %}

    {%- if resetpcs -%}
    int i;
    for (i=1; i<MAXPC; i++) {
        pc[tid][i] = 0;
    }{%- endif -%}

    pc[tid][{{exitpc}}] = {{exitvalue}};
}
