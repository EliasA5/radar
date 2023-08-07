#!/bin/bash

MYIPV4=$(hostname -I | awk -F' ' '{print $1}')
ERL_FLAGS='-config config/sys.config -args_file config/vm.args' rebar3 shell --apps radar --name radar@$MYIPV4
