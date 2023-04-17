void {{label}}(int tid) {
    {%-if labs-%}
    /*{%-for l in labs-%}{{l}}{%unless forloop.last%}; {%endunless%}{%-endfor-%}*/
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
    {%- endif -%}

    {%- if siblings.size > 0 %}    __CPROVER_assume({%- if last -%}
    {%- for item in siblings -%}(pc[tid][{{ item }}] == 0){% unless forloop.last %} && {% endunless %}{%- endfor -%}
    {%- else -%}
    {%- for item in siblings -%}(pc[tid][{{ item }}] != 0){%- unless forloop.last -%} || {% endunless %}{%- endfor -%}
    {%- endif -%});
    {%- endif -%}

    {%-if hasStigmergy and assignments-%}
    __CPROVER_assume((HoutCnt[tid] == 0) & (HinCnt[tid] == 0));
    {%-endif-%}
    {%-for guard in guards %}
    __CPROVER_assume({{ guard }});
    {%-endfor-%}

    {%- for l in locals-%}
    {%-if l.loc contains "Pick"-%}// ___symbolic-pick{%-if l.where != ""-%}-where{%endif%}___{%endif%}
    TYPEOFVALUES {{l.name}}{%-if l.size > 0-%}[{{l.size}}]{%-endif-%}; /* {{l.loc}} */
    {%-if l.loc contains "Pick" and l.size > 0-%}
    {%-for i in (1..l.size)-%}
    __CPROVER_assume(({{l.name}}[{{forloop.index0}}] >= {{l.pickFrom}}) & ({{l.name}}[{{forloop.index0}}] < {{l.pickTo}}) & ({{l.name}}[{{forloop.index0}}] != tid));
    {%-endfor-%}
    {%-capture allDifferent-%}
    {%-for i in (1..l.size)-%}{%- assign outer = forloop %}{%-for j in (1..i)-%}
    {%-if i != j-%}({{l.name}}[{{i | minus : 1}}] != {{l.name}}[{{j | minus : 1}}]) & {% endif-%}
    {%-endfor-%}{%-endfor-%}
    {%-endcapture-%}
    {%-if allDifferent != "" -%}__CPROVER_assume({{allDifferent}} 1);{%-endif-%}
    // ___end symbolic-pick{%-if l.where != ""-%}-where{%endif%}___
    {%-if l.where != ""-%}
    TYPEOFAGENTID __LABS_link1 = tid;

    for (unsigned char i = 0; i < {{l.size}}; ++i) {
        TYPEOFAGENTID __LABS_link2 = {{l.name}}[i];
        //__CPROVER_assume({{l.where}});
        __CPROVER_assume({{l.where}});
    }
    {%-endif-%}
    {%-endif-%}
    {%- endfor -%}

    {%- if assignments -%}
    {%- for a in assignments-%}{%- assign outer = forloop %}
    {%- for item in a -%}
    {%- if item.size != 0 -%}{% unless item.loc contains "Pick" %}
    {%if item.loc == "lstig"%}TYPEOFKEYLID{%elsif item.loc == "attr"%}TYPEOFKEYIID{%else%}TYPEOFKEYEID{%endif%} offset{{outer.index0}}_{{forloop.index0}} = {{item.offset}};
    // __CPROVER_assert(offset{{outer.index0}}_{{forloop.index0}} >= 0 && offset{{outer.index0}}_{{forloop.index0}} < {{item.size}}, "array bound");
    {%- endunless -%}{%- endif -%}
    {%- if item.loc == "Local"  -%}
    {{item.name}}{%- if item.size != 0 %}[offset{{forloop.index0}}]{% endif %} = {{item.expr}};
    {%-elsif item.loc == "attr" or item.loc == "lstig" or item.loc == "env"-%}
    TYPEOFVALUES val{{outer.index0}}_{{forloop.index0}} = {{item.expr}};
    {%-endif-%}
    {%- endfor -%}
    {%- for item in a -%}
    {%- capture check -%}{%- if outer.first and forloop.first -%}1{%- else -%}0{%- endif -%}{%- endcapture -%}
    {%-if item.loc == "attr" or item.loc == "lstig" or item.loc == "env"-%}
    {{item.loc}}(tid, {{item.key}}{%- if item.size != 0 %} + offset{{outer.index0}}_{{forloop.index0}}{% endif -%}, val{{outer.index0}}_{{forloop.index0}}, {{check}});
    {%- endif -%}{%- endfor -%}
    {%- endfor -%}
    {%- if hasStigmergy -%}
    {%- for qry in qrykeys -%}{%- for k in qry -%}
    setHin(tid, {{k}});
    {%- endfor -%}{%- endfor -%}
    {%- endif -%}
    {%- else -%}
    {%- if hasStigmergy -%}
    __CPROVER_assume(HoutCnt[tid] == 0);
    __CPROVER_assume(HinCnt[tid] == 0);
    {%- endif -%}
    {%- endif -%}

    
    {%- if sync and hasStigmergy -%}
    /* TODO
    {% if qrykeys.size > 0 -%}confirm();{% endif %}
    {% if assignments and assignments.first.loc == "lstig" -%}propagate();{% endif %}
    */
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
