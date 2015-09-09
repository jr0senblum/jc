#!/bin/bash

wget https://s3.amazonaws.com/rebar3/rebar3

cp -f travis/rebar.config rebar.config
cp -f travis/jc.app.src src/jc.app.src  

chmod a+x ./rebar3
./rebar3 update
./rebar3 ct 
