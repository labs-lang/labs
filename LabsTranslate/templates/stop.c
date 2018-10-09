void {{label}}(int tid) {

    {% include "templates/entry" with entrypoints %}

    term[tid] = 1;
    pc[tid][{{exitpc}}] = {{exitvalue}};
    
}
