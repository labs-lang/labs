void monitor() {
    {{alwaysasserts}}
}

void finally() {
    {{finallyasserts}}
    #ifdef SIMULATION
    assert(0);
    #endif
}

int main(void) {
    init();
    TYPEOFAGENTID firstAgent;

    unsigned __LABS_step;
    {% if fair -%}unsigned char last;{% endif %}
    for (__LABS_step=0; __LABS_step<BOUND; __LABS_step++) {
        // if (terminalState()) break;
        
        _Bool sys_or_not;

        if (sys_or_not) {
            LABSassume(firstAgent < MAXCOMPONENTS);

            {%- for item in schedule -%}
            {%- if forloop.first -%}
            if LABScheck({{ item.entry | join: " & " }}, {{ item.guards | join: " & " }}) {{ item.name }}(choice[__LABS_step]);
            {%- else -%}
            else if LABScheck({{ item.entry | join: " & " }}, {{ item.guards | join: " & " }}) {{ item.name }}(choice[__LABS_step]);
            {%- endif -%}
            {%- endfor -%}
            
            {%- if fair -%}
            if (firstAgent == MAXCOMPONENTS - 1) {
                firstAgent = 0;
            }
            else {
                firstAgent++;
            }
            {%- endif -%}
        }
        else {
            _Bool propagate_or_confirm; 

            if (propagate_or_confirm) propagate();
            else confirm();
        }
        monitor();
    }
    
    finally();
}

