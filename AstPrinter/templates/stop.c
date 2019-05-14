void {{label}}(int tid) {

    {% include "templates/entry" %}

    terminated[tid] = 1;

    {%- for item in exitpoints -%}
    pc[tid][{{ item.pc }}] = {{ item.value }};
    {%- endfor -%} 
}
