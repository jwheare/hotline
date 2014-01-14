#!/usr/bin/env sh
rebar compile && \
erl -pa apps/*/ebin -pa deps/*/ebin -boot start_sasl -s reloader -s websocket -s hotline
