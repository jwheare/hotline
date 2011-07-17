#!/usr/bin/env sh
rebar compile && \
erl -pa apps/hotline/ebin  -pa deps/*/ebin -boot start_sasl -s reloader -s hotline
