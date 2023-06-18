radar
=====

Distributed Radar project using msp430 devices with LDR and sonic sensors as radars, and connecting multiple devices using the distributed and fault tolerance of erlang.

The main branch is for the erlang source, the msp430 is for the msp c source and image.

Build
-----

    $ rebar3 compile

Run
-----

for the GUI:

    $ rebar3 shell --apps radar

for the workers/operators:

    $ rebar3 shell --apps operator

