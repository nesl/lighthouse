\begin{footnotesize}
\begin{verbatim}
TOS_MsgPtr receive(TOS_MsgPtr received, bool fromUART) {
    TOS_MsgPtr nextReceiveBuffer = received;

    dbg(DBG_USR1, "GenericBase received %s packet\n",
            fromUART ? "UART" : "radio");
    if ((!sendPending) &&
            (received->group == (TOS_AM_GROUP & 0xff))) {

        result_t ok;

        nextReceiveBuffer = ourBuffer;
        ourBuffer = received;
        dbg(DBG_USR1, "GenericBase forwarding packet to %s\n",
                fromUART ? "radio" : "UART");
        if (fromUART)
        {
            call Leds.redToggle();
            ok = call RadioSend.send(received);
        }
        else
        {
            call Leds.greenToggle();
            received->addr = TOS_UART_ADDR;
            ok = call UARTSend.send(received);
        }
        if (ok != FAIL)
        {
            dbg(DBG_USR1, "GenericBase send pending\n");
            sendPending = TRUE;
        }
        else {
            call Leds.yellowToggle();
        }

    }
    return nextReceiveBuffer;
}
\end{verbatim}
\end{footnotesize}

