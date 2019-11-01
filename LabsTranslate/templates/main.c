void monitor() {
    {%- for item in alwaysasserts -%}
    LABSassert({{item.value}}, {{item.name}});
    {%- endfor -%}
}

#if BOUND > 0
void finally() {
    {%- for item in finallyasserts -%}
    LABSassert({{item.value}}, {{item.name}});
    {%- endfor -%}
    #ifdef SIMULATION
    assert(0);
    #endif
}
#endif

int main(void) {
    init();
    TYPEOFAGENTID firstAgent{% if firstagent == 0 and fair %} = 0;{% else %};
    LABSassume(firstAgent < MAXCOMPONENTS);
    {% endif %};

    #if DISABLELSTIG == 0
        #if BOUND > 0
    Bool sys_or_not[BOUND];
        #endif
    #endif

    #if BOUND > 0
    unsigned char switchnondet[BOUND];
    unsigned __LABS_step;
    for (__LABS_step=0; __LABS_step<BOUND; __LABS_step++) {
    #else
    while(1) {        
    #endif
        // if (terminalState()) break;
        
        #if DISABLELSTIG == 0
            #if BOUND > 0
        if (sys_or_not[__LABS_step]) {
            #else
        if ((Bool) __VERIFIER_nondet()) {
            #endif
        #endif
            {%- unless fair -%}
            TYPEOFAGENTID nextAgent;
            LABSassume(nextAgent < MAXCOMPONENTS);
            firstAgent = nextAgent;
            {%- endunless -%}

            #if BOUND > 0
            switch (switchnondet[__LABS_step]) {
            #else
            switch (pc[firstAgent][0]) {
            #endif

            {%- for item in schedule -%}
                case {{ item.entry.first.value }}: {{ item.name }}(firstAgent); break;
            {%- endfor -%}
              default: 
                #if BOUND > 0
                LABSassume(0);
                #else
                {}
                #endif
            }
            
            {%- if fair -%}
            if (firstAgent == MAXCOMPONENTS - 1) {
                firstAgent = 0;
            }
            else {
                firstAgent++;
            }
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

        #if BOUND == 0
        {%- if finallyasserts -%}
        if ({%- for item in finallyasserts -%}{{item.value}}{%- endfor -%}) { 
            return 0; 
        }
        {%- endif -%}
        #endif

    }
    #if BOUND > 0
    finally();
    #endif
}

