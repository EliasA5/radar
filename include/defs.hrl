
-ifndef(DEFS_HRL).
-define(DEFS_HRL, true).

-define(DIST_SCALE, 2).

%% top 2 bytes of a message from communication to msp430
-define(MSPPC_ULTRASONIC,1).
-define(MSPPC_LDR,2).
-define(MSPPC_ACK,3).
-define(PCMSP_TELEMETER,1).
-define(PCMSP_FILE,2).
-define(PCMSP_COMMAND,0).

%% lower 6 bytes
-define(IDLE_CMD,0).
-define(TELEMETER_S_CMD,1).
-define(FILE_REC_S_CMD,2).
-define(SONIC_D_CMD,3).
-define(LDR_D_CMD,4).
-define(DUAL_D_CMD,5).
-define(FILE_0_CMD,6).
-define(FILE_1_CMD,7).
-define(FILE_2_CMD,8).

-endif.
