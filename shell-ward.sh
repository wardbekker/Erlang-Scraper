#!/bin/sh

rebar get-deps compile && erl -pa `pwd`/ebin `pwd`/deps/*/ebin -boot start_sasl +P 134217727 -run reloader
