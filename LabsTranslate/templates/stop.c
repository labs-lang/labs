void {{label}}(int tid) {

    {% include "templates/entry" with entrypoints %}

    terminated[tid] = 1;
    pc[tid][{{exitpc}}] = {{exitvalue}};
    
}
