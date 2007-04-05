This test demonstrates the limits of the checker to understand conditional
frees of data.  In this case, the function msg_send_senddone frees the data
payload depending on the value of a flag paramater.

This is logic that could be built into lighthouse, but would be very
specific to an implementation of SOS.  For now I will see how far I can get
without this functionality.

