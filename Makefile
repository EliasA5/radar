


# change the following line to the IP of the radar node
RADAR_IP := 192.168.1.16
MYIPV4 := $(shell hostname -I | awk -F' ' '{print $$1}')

shell:
	rebar3 shell

radar:
	ERL_FLAGS='-config config/sys.config -args_file config/vm.args' rebar3 shell --apps radar --name radar@$(MYIPV4)

operator:
	ERL_FLAGS='-config config/sys.config -args_file config/vm.args' RADAR_NODE=radar@$(RADAR_IP) rebar3 shell --apps operator --name operator@$(MYIPV4)

clean:
	rebar3 clean

