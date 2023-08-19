
Distributed Radar project using msp430 devices with LDR and sonic sensors as radars, and connecting multiple devices using the distributed and fault tolerance of erlang.

The main branch is for the erlang source, the msp430 is for the msp c source and image.

Compiling
-----
  - Download [Code Composer Studio](https://www.ti.com/tool/CCSTUDIO)
  - open a new project
  - clone this directory into the new project
  - build with CCS

Additionaly the latest compiled version already exists in this repository with the name `radar.out`.

Downloading on the MSP
-----

  - Use CCS to compile and load the binary directly to the MSP.
  - Use [mspdebug](https://github.com/dlbeer/mspdebug) to upload `radar.out` directly, by running the following command (one device connected at a time):
```bash
  mspdebug rf2500 "prog radar.out"
```

Pin Connections
-----

P1.3 (A3), P1.0 (A0) analog inputs from LDR
P2.0 (TA1.0) trigger, P2.2 (TA1.1) pwm, P2.4 (TA1.2) Echo for ultrasonic sensor
LCD Ctl Pins P2.5 P2.6 P2.7
LCD Data P1.4 - P1.7
