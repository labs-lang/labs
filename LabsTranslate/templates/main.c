int main(void) {
    init();
    unsigned char choice[BOUND];
    int __LABS_step;
    {% if fair -%}unsigned char last;{% endif %}
    for (__LABS_step=0; __LABS_step<BOUND; __LABS_step++) {
        if (all_term()) break;
    
        __VERIFIER_assume(choice[__LABS_step] < MAXCOMPONENTS + 2);
    
        if (choice[__LABS_step] < MAXCOMPONENTS) {
            {% if fair -%}__VERIFIER_assume(choice[__LABS_step] == last+1 || (last == MAXCOMPONENTS - 1 && choice[__LABS_step] == 0));{% endif %}
{{schedule}}
            {% if fair -%}last = choice[__LABS_step];{% endif %}
        }
        else if (choice[__LABS_step] == MAXCOMPONENTS) 
            propagate();
        else if (choice[__LABS_step] == MAXCOMPONENTS + 1)
            confirm();
        monitor();
    }
    
{{finallyasserts}}
    
}

