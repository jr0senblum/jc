#!/bin/bash

wget https://s3.amazonaws.com/rebar3/rebar3

chmod a+x ./rebar3
./rebar3 update
./rebar3 compile
./rebar3 ct 
