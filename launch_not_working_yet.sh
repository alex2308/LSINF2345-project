#!/bin/sh


#
# This script makes it easier to launch the distributed storage system.
# It launches the distributed system localy or open as a Node in a network.
# no shell
#
# USAGE:
# ./launch 0 SHELL NODENAME NUMBERSTORES DATANAME
# ./launch 1 SHELL NODENAME STORENAME STORENODE MANAGERNAME
# ./launch 2 SHELL NODENAME MANAGERNAME MANAGERNODE
#
#
# ex. ./launch 1 manager manager => opens a erlang terminal with running node with name manager@HOST
#


if [ "$#" -ne 5 or "$#" -ne 6 ]; then
	echo "illegal number of parameters";
else
    # run a datastore
    if [ $1 -eq 0 ]; then
        if [ $2 -eq 0 ]; then # open no shell
            erl -pa bin/ -sname $3 -run datastore start $4 $5 -noshell;
        else
            erl -pa bin/ -sname $3 -run datastore start $4 $5;
        fi
    fi

    # run a tmanager
    if [ $1 -eq 1 ]; then
        if [ $2 -eq 0 ]; then # open no shell
            erl -pa bin/ -sname $3 -run tmanager start $4 $5 $6 -noshell;
        else
            erl -pa bin/ -sname $3 -run tmanager start $4 $5 $6;
        fi
    fi

    # run an app
    if [ $1 -eq 2 ]; then
        if [ $2 -eq 0 ]; then # open no shell
            erl -pa bin/ -sname $3 -run app start $4 $5 -noshell;
        else
            erl -pa bin/ -sname $3 -run app start $4 $5;
        fi
    fi
fi
