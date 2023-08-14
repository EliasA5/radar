radar
=====

Distributed Radar project using msp430 devices with LDR and sonic sensors as radars, and connecting multiple devices using the distributed and fault tolerance of erlang.

The main branch is for the erlang source, the msp430 is for the msp c source and image.

Build
-----

    $ rebar3 compile

Run
-----

Our project will only run on linux machines since we used the inotify subsystem which only exists on linux (there are
equivalents in mac and other UNIX-like OS’s but not implemented in the module we used).
We also need a modern Erlang installation with rebar3 and wxWidgets (tested with Erlang/OTP 24-26).

Using the Makefile:

```bash
git clone https://github.com/EliasA5/radar
cd radar
make radar
```

Which will open the gui in distributed mode, to connect operators to it we need to change the RADAR IP field inside the
Makefile, to the one given in the shell that opened the radar, i.e in the shell that we ran `make radar`:

```erlang
Erlang/OTP 25 [erts-13.2.2.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Eshell V13.2.2.2  (abort with ^G)
(radar@10.0.0.24)1> ===> Booted radar
```
We set RADAR\_IP in the Makefile to:

    RADAR_IP := 10.0.0.24

then we simply run in another shell:

    make operator

and of course we can take the commands from the Makefile and run them manually to give the radar/operators different names,
taking care to set the correct ip and cookie for all.

for the GUI:

    ERL_FLAGS='-config config/sys.config' rebar3 shell --apps radar --name radar@<ip> --setcookie <cookie>

for the workers/operators:

    ERL_FLAGS='-config config/sys.config' RADAR_NODE=radar@<ip> rebar3 shell --apps operator --name <oprator_name>@<ip> --setcookie <cookie>

To generate new imaginary radars, we create a new directory called ”dev/” inside the project root directory (created automatically after starting the operator), and inside it we create new files with the names ”radar_[0-9a-zA-Z]+”, and to remove the
radar we simply delete it’s file from this directory, for example:
```bash
.../radar> cd dev
.../radar/dev> touch radar_12hi # creates a new radar
.../radar/dev> touch radar_{000..100} # creates 101 new radars
.../radar/dev> rm radar_12hi # remove the radar
.../radar/dev> rm radar_* # remove all radars
```
