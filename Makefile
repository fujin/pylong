

.PHONY: deps test

all: deps compile

run:
	erl -pa ebin deps/*/ebin \
	-mode interactive \
	-boot start_sasl \
	-eval "application:load(pylon), riak_core_util:start_app_deps(pylon)." \
	-name pylon -setcookie pylon

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps

test: all
	./rebar skip_deps=true eunit

docs: deps
	./rebar skip_deps=true doc

dialyzer: compile
	@dialyzer -Wno_return -c apps/riak_core/ebin
