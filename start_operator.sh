#!/bin/bash

# change the following line to the IP of the radar node
RADAR_IP=192.168.1.16
MYIPV4=$(hostname -I | awk -F' ' '{print $1}')
ERL_FLAGS='-config config/sys.config -args_file config/vm.args' RADAR_NODE=radar@$RADAR_IP rebar3 shell --apps operator --name operator@$MYIPV4
