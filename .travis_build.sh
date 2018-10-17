#!/bin/bash

#wget https://s3.amazonaws.com/rebar3/rebar3

chmod a+x ./rebar3
./rebar3 unlock
./rebar3 update

epmd -daemon

./rebar3 ct 
