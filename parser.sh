#!/usr/bin/env bash


if [ "$#" -eq 2 ]; then
	make clean
	make all
    erl -pa bin/ -run parser start_shell $1 $2 -noshell -noinput;
else
    echo "Uncorrect number of arguments"
    echo "USAGE: ./parser.sh InputFilename OutputFilename "
fi
