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
        #if BOUND > 0
    Bool sys_or_not[BOUND];
        #endif
    #endif

    #if BOUND > 0
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
            LABSassume(firstAgent < MAXCOMPONENTS);

            #if BOUND > 0
            switch (switchnondet[__LABS_step]) {
            #else
                switch (__VERIFIER_nondet()) {
            #endif

            {%- for item in schedule -%}
                case {{ forloop.index0 }}: {{ item.name }}(firstAgent); break;
            {%- endfor -%}
              default: LABSassume(0);
            }
            
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

