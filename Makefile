# This is normally done by activating kerl using:
#  $ source /opt/erlang/19.3/activate
export PATH := /opt/erlang/19.3/bin:$(PATH)


compile:
	rebar3 as dev release

distclean:
	rm -Rf ./_build

start: compile
	./_build/dev/rel/hotline/bin/hotline console

.PHONY: compile distclean start
