{% if eventuallypredicates.size > 0 %}
_Bool __LABS_eventually = 0;
{%endif%}

{%-for item in otherproperties-%}
{%-if item.scope.type == "between"-%}_Bool __LABS_{{item.name}}_isOpen = 0;{%-endif-%}
{%-if item.scope.type == "fromUntil"-%}_Bool __LABS_{{item.name}}_isOpen = 0;{%-endif-%}
{%-if item.modality == "thereIs"-%}
_Bool __LABS_{{item.name}}_isSat = 0;
{%-endif-%}
{%-if item.modality == "always"-%}
_Bool __LABS_{{item.name}}_isSat = 1;
{%-endif-%}
{%-endfor-%}

void monitor(void) {
    {% if eventuallypredicates.size > 0 %}
    {%if simulation%}
    __sim_satisfied({{eventuallypredicates.first.value}}, "{{eventuallypredicates.first.name}}");
    {%else%}
    if (!__LABS_eventually & ({{eventuallypredicates.first.value}})) {
         __LABS_eventually = 1;
    }
    {%endif%}
    {%endif%}    
    {%- for item in alwaysasserts -%}
    {%-if simulation-%}
    __sim_assert({{item.value}}, "{{item.name}}");
    {%-else-%}
    __CPROVER_assert({{item.value}}, "{{item.name}}");
    {%-endif-%}
    {%- endfor -%}


    {%-for item in otherproperties-%}
    // ------------------------------------------------------------------------
    // {{ item.name}}:
    // {{ item.scope.type }} {{ item.scope.open }} and {{ item.scope.close }}
    {%-if item.modality == "precedes"%}    // {{ item.prec }} precedes {{ item.predicate }}
    {%-else%}    // {{ item.modality }} {{ item.predicate }}
    {%-endif-%}
    // If both open and close hold, the scope is null and {{item.name}} passes vacuously
    if ({{ item.scope.open }} & !({{ item.scope.close }})) __LABS_{{item.name}}_isOpen = 1;
    {%-if item.modality == "thereIs"-%}
    if (__LABS_{{item.name}}_isOpen & !({{ item.scope.close }}) & ({{ item.predicate }})) __LABS_{{item.name}}_isSat = 1;
    {%-endif-%}
    {%-if item.modality == "always" -%}
    if (__LABS_{{item.name}}_isOpen & !({{ item.scope.close }}) & !({{ item.predicate }})) __LABS_{{item.name}}_isSat = 0;
    {%-endif-%}
    // Closing the scope
    if (__LABS_{{item.name}}_isOpen & ({{ item.scope.close }})) {
        __LABS_{{item.name}}_isOpen = 0;
        __CPROVER_assert(__LABS_{{item.name}}_isSat, "{{item.name}}");
    }
    // ------------------------------------------------------------------------
    {%-endfor-%}

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
    {%- else -%}
    {% if eventuallypredicates.size > 0 %}
    {%-unless simulation-%}
    __CPROVER_assert(__LABS_eventually, "{{eventuallypredicates.first.name}}");
    {%-endunless-%}
    {%endif%}
    {%- endif -%}
}
{%- endif -%}


void __invariants(void) { }

int main(void) {
    init();
    monitor(); // Check invariants on the initial state
    TYPEOFAGENTID scheduled = 0;
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

            {%-unless fair-%}
            // ___symbolic-scheduler___
            scheduled = __CPROVER_nondet();
            scheduled = ((scheduled >= 0) & (scheduled < MAXCOMPONENTS)) ? scheduled : 0;
            // ___end symbolic-scheduler___
            {%-endunless-%}

            switch (pc[scheduled][0]) {
            {%- for item in schedule -%}
                case {{ item.entry.first.value }}: {{ item.name }}(scheduled); break;
            {%- endfor -%}
              default: 
                __CPROVER_assume(0);
            }
            
            {%- if fair -%}
            // ___symbolic-scheduler___
            scheduled = scheduled == MAXCOMPONENTS - 1 ? 0 : scheduled + 1;
            // ___end symbolic-scheduler___
            {%-endif-%}
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
        {%- if eventuallypredicates.size > 0 and bound == 0 -%}
        if ({{eventuallypredicates.first.value}}) { 
            return 0; 
        }
        {%- endif -%}
    }

    {%- if bound > 0 -%}
    {%- if simulation -%}
    finally();
    {%- elsif eventuallypredicates.size > 0 or finallyasserts.size > 0 and bound > 0 -%}
    finally();
    {%- endif -%}
    {%- endif -%}

}

///
{{pcmap}}
///