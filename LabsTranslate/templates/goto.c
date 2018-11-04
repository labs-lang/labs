void {{label}}(int tid) {
    {% if labs %}//{{labs}}
    {%- endif -%}
{% include "templates/entry" %}

    {%- if assignments -%}{%- for item in assignments -%}
    int val{{forloop.index0}} = {{item.expr}};
    {%- if item.size != 0 -%}
    int offset{{forloop.index0}} = {{item.offset}};
    assert(offset{{forloop.index0}} >= 0 && offset{{forloop.index0}} < {{item.size}});
    {%- endif -%}{%- endfor -%}

    {%- for item in assignments -%}
    {%- capture check -%}{%- if forloop.first -%}1{%- else -%}0{%- endif -%}{%- endcapture -%}
    {%- if item.size != 0 -%}
    {{type}}(tid, {{item.key}} + offset{{forloop.index0}}, val{{forloop.index0}}, {{check}});
    {%- else -%}
    {{type}}(tid, {{item.key}}, val{{forloop.index0}}, {{check}});
    {%- endif -%}{%- endfor -%}
    {%- for k in qrykeys -%}
    setHin(tid, {{k}});{%- endfor -%}{%- endif -%}

    {%- for item in exitpoints -%}
    {%- if item.values.size == 1-%}
    pc[tid][{{ item.pc }}] == {{ item.values.first }};
    {%- else -%}
    int pc{{item.pc}};
    LABSassume({%- for val in item.values -%} pc{{ item.pc }} == {{ val }}{% unless forloop.last %} || {% endunless %}{%- endfor-%});
    pc[tid][{{ item.pc }}] == pc{{ item.pc }};
    {%-endif-%}{%- endfor -%}

}
