void monitor() {
    {%- for item in alwaysasserts -%}
    LABSassert({{item}});
    {%- endfor -%}
}

void finally() {
    {%- for item in finallyasserts-%}
    LABSassert({{item}});
    {%- endfor -%}
    #ifdef SIMULATION
    assert(0);
    #endif
}

int main(void) {
    init();
    TYPEOFAGENTID firstAgent{% if firstagent == 0 and fair %} = 0{% endif %};
    Bool sys_or_not[BOUND];


    unsigned __LABS_step;
    for (__LABS_step=0; __LABS_step<BOUND; __LABS_step++) {
        // if (terminalState()) break;
        
        // _Bool sys_or_not;

        if (sys_or_not[__LABS_step]) {
            LABSassume(firstAgent < MAXCOMPONENTS);

            {%- for item in schedule -%}
            {% unless forloop.first %}else {% endunless %}if LABScheck({%- for pc in item.entry -%}
pc[firstAgent][{{pc.name}}] == {{pc.value}}{% unless forloop.last %} & {% endunless %}{%- endfor -%}, {{ item.guards | join: " & " }}) {{ item.name }}(firstAgent);
{%- endfor -%}
            
            {%- if fair -%}
            if (firstAgent == MAXCOMPONENTS - 1) {
                firstAgent = 0;
            }
            else {
                firstAgent++;
            }
            {%- else -%}
            firstAgent = nondet();
            {%- endif -%}
        }
        else {
            Bool propagate_or_confirm; 

            if (propagate_or_confirm) propagate();
            else confirm();
        }
        monitor();
    }
    
    finally();
}

