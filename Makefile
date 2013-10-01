.PHONY: rel deps test

all: deps compile

compile: deps
	@./rebar compile

app:
	@./rebar compile skip_deps=true

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

test: all test_erlang test_javascript

test_erlang: all
	@./rebar skip_deps=true eunit

define MISSING_PHANTOM_MESSAGE
PhantomJS is missing to run the javascript tests, to install on your OS do the following

- MacOS via homebrew run `brew install phantomjs`
- Ubuntu `apt-get install phantomjs`
- Other linux distros should check http://phantomjs.org/download.html to download a binary build

or visit http://phantomjs.org/ for more information.
endef
export MISSING_PHANTOM_MESSAGE

test_javascript: all
ifeq ($(strip $(shell command -v phantomjs > /dev/null 2>&1 || echo 1)),)
	@echo "==> riak_control (qunit)"
	@phantomjs test/javascripts/runner.js test/javascripts/index.html
else
	@echo "$$MISSING_PHANTOM_MESSAGE"
endif

##
## Doc targets
##
docs:
	./rebar skip_deps=true doc

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool eunit syntax_tools compiler mnesia public_key snmp
PLT = $(HOME)/.riak_control_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(PLT) --apps $(APPS) deps/webmachine/ebin deps/mochiweb/ebin deps/erlydtl/ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(PLT) --apps $(APPS) deps/webmachine/ebin deps/mochiweb/ebin deps/erlydtl/ebin

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return -Wunmatched_returns --plt $(PLT) ebin

clean_plt:
	@echo
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(PLT)

typer:
	typer --annotate -I ../ --plt $(PLT) -r src
