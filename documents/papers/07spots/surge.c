\begin{footnotesize}
\begin{verbatim}
01 int8_t surge_module(void *state, Message *msg)
02 {
03   surge_state_t *s = (surge_state_t*)state;
04
05   switch (msg->type){
06   case MSG_DATA_READY: // Requested sensor data ready
07   {
08     SurgeMsg* pkt =
09       (uint8_t*)ker_malloc(sizeof(SurgeMsg));
10     if (pkt == NULL) break;
11     pkt->data = ... ; // ... set up message
12     post_long(TREE_ROUTING_PID, SURGE_MOD_PID,
13               MSG_SEND_PACKET, sizeof(SurgeMsg),
14               (void*)pkt, SOS_MSG_RELEASE);
15     break;
16   }
17   case MSG_TR_DATA_PKT:
18   {
19     if (ker_id() == SURGE_BASE_STATION_ADDRESS){
20       uint8_t *payload = ker_msg_take_data(msg);
21       post_net(SURGE_MOD_PID, SURGE_MOD_PID,
22                msg->type, msg->len, payload,
23                SOS_MSG_RELEASE, ker_uart_id());
24       return SOS_OK;
25     }
26     break;
27   }
28   case ...: { ...; break; } // other messages
29   }
30   return SOS_OK;
31 }
\end{verbatim}
\end{footnotesize}
