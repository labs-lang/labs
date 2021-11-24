void monitor(void) {
    {%- for item in alwaysasserts -%}
    {%-if simulation-%}
    __sim_assert({{item.value}}, "{{item.name}}");
    {%-else-%}
    __CPROVER_assert({{item.value}}, "{{item.name}}");
    {%-endif-%}
    {%- endfor -%}
}

{%- if bound > 0 -%}
void finally(void) {
    {%- for item in finallyasserts -%}
    {%-if simulation-%}
    __sim_assert({{item.value}}, "{{item.name}}");
    {%-else-%}
    __CPROVER_assert({{item.value}}, "{{item.name}}");
    {%-endif-%}
    {%- endfor -%}
    {%- if simulation -%}
    __CPROVER_assert(0, "__sliver_simulation__");
    {%- endif -%}
}
{%- endif -%}

int main(void) {
    init();
    monitor(); // Check invariants on the initial state
    TYPEOFAGENTID firstAgent{% if firstagent == 0 and fair %} = 0;{% else %};
    __CPROVER_assume(firstAgent < MAXCOMPONENTS);
    {% endif %};
    {%- if hasStigmergy -%}
    _Bool propagate_or_confirm = 0;
    {%- endif -%}

    {%- if hasStigmergy and bound > 0 -%}
    _Bool sys_or_not[BOUND];
    {%- endif -%}

    {%- if bound > 0 -%}
    for (__LABS_step=0; __LABS_step<BOUND; __LABS_step++) {
    {%- else -%}
    while(1) {        
    {%- endif -%}
        
        {%- if hasStigmergy -%}{%- if bound > 0 -%}
        if (sys_or_not[__LABS_step]) {
        {%- else -%}
        if ((_Bool) __CPROVER_nondet()) {
        {%- endif -%}{%- endif -%}
            // ___concrete-scheduler___
            // ___end concrete-scheduler___

            switch (pc[firstAgent][0]) {
            {%- for item in schedule -%}
                case {{ item.entry.first.value }}: {{ item.name }}(firstAgent); break;
            {%- endfor -%}
              default: 
                {%- if bound > 0 -%}
                __CPROVER_assume(0);
                {%- else -%}
                {}
                {%- endif -%}
            }
            
            // ___symbolic-scheduler___
            {%- if fair -%}
            if (firstAgent == MAXCOMPONENTS - 1) {
                firstAgent = 0;
            }
            else {
                firstAgent++;
            }
            {%-else-%}
            TYPEOFAGENTID nextAgent = __CPROVER_nondet();
            __CPROVER_assume(nextAgent < MAXCOMPONENTS);
            firstAgent = nextAgent;
            {%-endif-%}
            // ___end symbolic-scheduler___
        {%- if hasStigmergy -%}
        }
        else {
            propagate_or_confirm = __CPROVER_nondet(); 
            if (propagate_or_confirm) propagate();
            else confirm();
        }
        {%- endif -%}
        monitor();

        {%- if finallyasserts and finallyasserts.size > 0 and bound == 0 -%}
        if ({%- for item in finallyasserts -%}{{item.value}}{%- endfor -%}) { 
            return 0; 
        }
        {%- endif -%}
    }
    {%- if simulation or (finallyasserts and finallyasserts.size > 0 and bound > 0 -%}
    finally();
    {%- endif -%}

}
