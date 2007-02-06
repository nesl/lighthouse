stores {
    global_buff;
};



create_a.pre {
    global_buff.empty();
}

create_a.post {
    global_buff.full();
}

delete_a.pre {
    global_buff.full();
}

delete_a.post {
    global_buff.empty();
}



create_b.pre {
    global_buff.empty();
}

create_b.post {
    global_buff.full();
}

delete_b.pre {
    global_buff.full();
}

delete_b.post {
    global_buff.empty();
}

