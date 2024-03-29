void {{label}}(int tid) {
    {% if labs %}//{{labs}}
    {%- endif -%}
    {%-if ifCond != ""-%}
    if (!({{ifCond}})) {
        {%- for item in ifExit -%}
        {%- if item.value.size == 1-%}
        pc[tid][{{ item.name }}] = {{ item.value.first }};
        {%- else -%}
        TYPEOFPC pc{{item.name}};
        __CPROVER_assume({%- for val in item.value -%} (pc{{ item.name }} == {{ val }}){% unless forloop.last %} | {% endunless %}{%- endfor-%});
        pc[tid][{{ item.name }}] = pc{{ item.name }};
        {%- endif -%}{%- endfor -%}
        return;
    }
    {%-endif-%}
    
    {%- include "templates/entry" -%}

    {%- if assignments -%}{%- for item in assignments -%}
    TYPEOFVALUES val{{forloop.index0}} = {{item.expr}};
    {%- if item.size != 0 -%}
    {% if item.loc == "lstig" %}TYPEOFKEYLID{% elsif item.loc == "attr" %}TYPEOFKEYIID{% else %}TYPEOFKEYEID{% endif %} offset{{forloop.index0}} = {{item.offset}};
    // TODO make array bound checks optional (e.g., only if --debug is set on the sliver CLI)
    // __CPROVER_assert(offset{{forloop.index0}} >= 0 && offset{{forloop.index0}} < {{item.size}}, "array bound");
    {%- endif -%}{%- endfor -%}

    {%- for item in assignments -%}
    {%- capture check -%}{%- if forloop.first -%}1{%- else -%}0{%- endif -%}{%- endcapture -%}
    {%- if item.size != 0 -%}
    {{item.loc}}(tid, {{item.key}} + offset{{forloop.index0}}, val{{forloop.index0}}, {{check}});
    {%- else -%}
    {{item.loc}}(tid, {{item.key}}, val{{forloop.index0}}, {{check}});
    {%- endif -%}{%- endfor -%}
    {%- for k in qrykeys -%}
    setHin(tid, {{k}});
    {%- endfor -%}
    {%- else -%}
    {%- if hasStigmergy -%}
    __CPROVER_assume(HoutCnt[tid] == 0);
    __CPROVER_assume(HinCnt[tid] == 0);
    {%- endif -%}
    {%- endif -%}

    {%- if sync -%}
    {% if qrykeys.size > 0 -%}confirm();{% endif %}
    {% if assignments.first.loc == "lstig" -%}propagate();{% endif %}
    {%- endif -%}

    {%- for item in exitcond -%}
    {%- if item.value.size == 1-%}
    pc[tid][{{ item.name }}] = {{ item.value.first }};
    {%- else -%}
    TYPEOFPC pc{{item.name}};
    __CPROVER_assume({%- for val in item.value -%} (pc{{ item.name }} == {{ val }}){% unless forloop.last %} | {% endunless %}{%- endfor-%});
    pc[tid][{{ item.name }}] = pc{{ item.name }};
    {%- endif -%}{%- endfor -%}

}
