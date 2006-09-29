if( post_long(fst->requester, KER_FETCHER_PID,
                          MSG_FETCHER_DONE, sizeof(fetcher_state_t),
                                        fst, SOS_MSG_RELEASE) != SOS_OK ) {
        ...
                return;
}
cam = ker_cam_lookup( fst->map.key );
...

