void {{label}}(int tid) {
    {% include "templates/entry" %}

    {%- for item in exitpoints -%}
    pc[tid][{{ item.pc }}] = {{ item.value }};
    {%- endfor -%}
}
