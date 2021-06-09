compile:
	rebar3 as dev release -n hotline -v dev

distclean:
	rm -Rf ./_build

start: compile
	./_build/dev/rel/hotline/bin/hotline console

.PHONY: compile distclean start
