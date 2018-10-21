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
    unsigned char choice[BOUND];
    unsigned char agent_choice[BOUND];


    int __LABS_step;
    {% if fair -%}unsigned char last;{% endif %}
    for (__LABS_step=0; __LABS_step<BOUND; __LABS_step++) {
        if (terminalState()) break;
        
        LABSassume(choice[__LABS_step] < MAXCOMPONENTS + 2);
    
        if (choice[__LABS_step] < MAXCOMPONENTS) {
            LABSassume(agent_choice[__LABS_step] < {{ schedule.size }});
            {%- if fair -%}
            LABSassume(choice[__LABS_step] == last+1 || (last == MAXCOMPONENTS - 1 && choice[__LABS_step] == 0));
            {%- endif -%}

            {%- for item in schedule -%}
            {%- if forloop.first -%}
            if (agent_choice[__LABS_step] == {{forloop.index0}}) {{item}}(choice[__LABS_step]);
            {%- else -%}
            else if ((agent_choice[__LABS_step] == {{forloop.index0}})) {{item}}(choice[__LABS_step]);
            {%- endif -%}
            {%- endfor -%}
            
            {%- if fair -%}
            last = choice[__LABS_step];
            {%- endif -%}
        }
        else if (choice[__LABS_step] == MAXCOMPONENTS) 
            propagate();
        else if (choice[__LABS_step] == MAXCOMPONENTS + 1)
            confirm();
        monitor();
    }
    
    finally();
}

