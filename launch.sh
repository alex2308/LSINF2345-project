#!/bin/bash
#
# Script used to launch a live node for a transactional manager
# or for a datastore
#
# ./launch db dbnodename numbersofdb dbconnectorname
# ./launch tm tmnodename dbnodename dbconnectorname tmname
#
#USAGE:
#    ./launch db dbnodename numbersofdb dbconnectorname
#        -Start a live datastore node 'dbnodename' which contains 'numbersofdb' datastores and registers the connector as 'dbconnectorname'
#    ./launch tm tmnodename dbnodename dbconnectorname tmname
#        -Start a live transactional manager node 'tmnodename' which will connect to a db located in node 'dbnodename' with connector name 'dbconnectorname'
#
#Erlang shells will stay open. To terminated the db or tm, just close the shell or send a {exit} message to the registered tm/db names
#

bold=$(tput bold)
normal=$(tput sgr0)

usage="${bold}USAGE${normal}:
    ${bold}./launch db dbnodename numbersofdb dbconnectorname${normal}
        -Start a live datastore node 'dbnodename' which contains 'numbersofdb' datastores and registers the connector as 'dbconnectorname'
    ${bold}./launch tm tmnodename dbnodename dbconnectorname tmname${normal}
        -Start a live transactional manager node 'tmnodename' which will connect to a db located in node 'dbnodename' with connector name 'dbconnectorname'

Erlang shells will stay open. To terminated the db or tm, just close the shell or send a {exit} message to the registered tm/db names"
if [ "$#" -eq 0 ]; then
    echo -e "$usage"
else
    if [ "$1" == "db" ]; then
        if [ "$#" -eq 4 ]; then
            make all
           erl -pa bin/ -sname $2 -run datastore start_shell $3 $4
        else
            echo "incorrect arguments"
            echo -e "$usage"
        fi
    else
        if [ "$1" == "tm" ]; then
            if [ "$#" -eq 5 ]; then
                make all
                erl -pa bin/ -sname $2 -run tmanager start_shell $4 $3 $5
            else
                echo "incorrect arguments"
                echo -e "$usage"
            fi
        else
            echo "incorrect arguments"
            echo -e "$usage"
        fi
    fi
fi
