#!/usr/bin/env sh
erl -pa apps/hotline/ebin -boot start_sasl -s reloader -s hotline
