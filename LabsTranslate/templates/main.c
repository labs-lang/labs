void monitor() {
    {%- for item in alwaysasserts -%}
    LABSassert({{item.value}}, {{item.name}});
    {%- endfor -%}
}

void finally() {
    {%- for item in finallyasserts -%}
    LABSassert({{item.value}}, {{item.name}});
    {%- endfor -%}
    #ifdef SIMULATION
    assert(0);
    #endif
}

int main(void) {
    init();
    TYPEOFAGENTID firstAgent{% if firstagent == 0 and fair %} = 0{% endif %};

    #if DISABLELSTIG == 0
    Bool sys_or_not[BOUND];
    #endif

    unsigned __LABS_step;
    for (__LABS_step=0; __LABS_step<BOUND; __LABS_step++) {
        // if (terminalState()) break;
        
        // _Bool sys_or_not;
        #if DISABLELSTIG == 0
        if (sys_or_not[__LABS_step]) {
        #endif
            LABSassume(firstAgent < MAXCOMPONENTS);

            {%- for item in schedule -%}
            {% unless forloop.first %}else {% endunless %}if LABScheck({%- for pc in item.entry -%}
pc[firstAgent][{{pc.name}}] == {{pc.value}}{% unless forloop.last %} & {% endunless %}{%- endfor -%}
{%- if item.siblings.size != 0 -%} &
{%- for pc in item.siblings -%}
pc[firstAgent][{{pc}}] {%-if item.name contains 'last'-%}=={%- else -%}!={%- endif -%} 0{% unless forloop.last %} & {% endunless %}
{%- endfor -%}
{%- endif -%}
, {{ item.guards | join: " & " }}) {{ item.name }}(firstAgent);
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
        #if DISABLELSTIG == 0 
        }
        else {
            Bool propagate_or_confirm; 

            if (propagate_or_confirm) propagate();
            else confirm();
        }
        #endif
        monitor();
    }
    
    finally();
}

