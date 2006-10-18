\begin{footnotesize}
\begin{verbatim}
int8_t surge_module(void *state, Message *msg)
{
  surge_state_t *s = (surge_state_t*)state;

  switch (msg->type){
  case MSG_DATA_READY: // Requested sensor data ready
  {
    SurgeMsg* pkt = 
      (uint8_t*)ker_malloc(sizeof(SurgeMsg));
    if (pkt == NULL) break; 
    pkt->data = ... ; // ... set up message
    post_long(TREE_ROUTING_PID, SURGE_MOD_PID, 
              MSG_SEND_PACKET, sizeof(SurgeMsg), 
              (void*)pkt, SOS_MSG_RELEASE);
    break;
  }
  case MSG_TR_DATA_PKT: 
  {
    if (ker_id() == SURGE_BASE_STATION_ADDRESS){
      uint8_t *payload = ker_msg_take_data(msg); 
      post_net(SURGE_MOD_PID, SURGE_MOD_PID,
               msg->type, msg->len, payload,
               SOS_MSG_RELEASE, ker_uart_id());
      return SOS_OK;
    }
    break;
  }
  case ...: { ...; break; } // other messages
  }
  return SOS_OK;
}
\end{verbatim}
\end{footnotesize}
