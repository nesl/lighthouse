\begin{scriptsize}
\begin{verbatim}
01 TOS_MsgPtr receive(TOS_MsgPtr received, bool fromUART) {
02     TOS_MsgPtr nextReceiveBuffer = received;
03
04     dbg(DBG_USR1, "GenericBase received %s packet\n",
05             fromUART ? "UART" : "radio");
06     if ((!sendPending) &&
07             (received->group == (TOS_AM_GROUP & 0xff))) {
08         result_t ok;
09
10         nextReceiveBuffer = ourBuffer;
11         ourBuffer = received;
12         dbg(DBG_USR1, "GenericBase forwarding packet to %s\n",
13                 fromUART ? "radio" : "UART");
14         if (fromUART) {
15             call Leds.redToggle();
16             ok = call RadioSend.send(received);
17         } else {
18             call Leds.greenToggle();
19             received->addr = TOS_UART_ADDR;
20             ok = call UARTSend.send(received);
21         }
22         if (ok != FAIL) {
23             dbg(DBG_USR1, "GenericBase send pending\n");
24             sendPending = TRUE;
25         } else {
26             call Leds.yellowToggle();
27         }
28     }
29     return nextReceiveBuffer;
30 }
\end{verbatim}
\end{scriptsize}
