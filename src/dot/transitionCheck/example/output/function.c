
enum State {
    dfs_MSG_INIT,
    dfs_end,
    dfs_MSG_TIMER_TIMEOUT__MEMORY_TID__S0,
    dfs_MSG_TIMER_TIMEOUT__MEMORY_TID__S1,
    dfs_MSG_FINAL__S0,
    dfs_start,
    dfs_MSG_FINAL__S1,
};

bool valid_transition(enum State old, enum State new) {

    if (old == dfs_MSG_INIT) {
        if (new == dfs_MSG_TIMER_TIMEOUT__MEMORY_TID__S0) {
            return true;
        } else {
            return false;
        }
    }

    if (old == dfs_end) {
        {
            return false;
        }
    }

    if (old == dfs_MSG_TIMER_TIMEOUT__MEMORY_TID__S0) {
        if (new == dfs_MSG_FINAL__S0) {
            return true;
        } else if (new == dfs_MSG_TIMER_TIMEOUT__MEMORY_TID__S1) {
            return true;
        } else {
            return false;
        }
    }

    if (old == dfs_MSG_TIMER_TIMEOUT__MEMORY_TID__S1) {
        if (new == dfs_MSG_FINAL__S1) {
            return true;
        } else if (new == dfs_MSG_TIMER_TIMEOUT__MEMORY_TID__S0) {
            return true;
        } else {
            return false;
        }
    }

    if (old == dfs_MSG_FINAL__S0) {
        if (new == dfs_end) {
            return true;
        } else {
            return false;
        }
    }

    if (old == dfs_start) {
        if (new == dfs_MSG_INIT) {
            return true;
        } else {
            return false;
        }
    }

    if (old == dfs_MSG_FINAL__S1) {
        if (new == dfs_end) {
            return true;
        } else {
            return false;
        }
    }

    return false;
}
