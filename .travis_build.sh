#!/bin/bash

wget https://s3.amazonaws.com/rebar3/rebar3

cp -f travis/rebar.config rebar.config
cp -f travis/jc.app.src src/jc.app.src  

chmod a+x ./rebar3
./rebar3 unlock
./rebar3 update
epmd -names
env DEBUG=1 ./rebar3 ct 
