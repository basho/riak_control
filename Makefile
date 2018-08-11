.PHONY: rel deps test

all: deps compile

compile: deps
	@./rebar3 compile

app:
	@./rebar3 compile skip_deps=true

deps:
	@./rebar3 get-deps

clean:
	@./rebar3 clean

distclean: clean
	@./rebar3 delete-deps

test: all test_erlang test_javascript

test_erlang: all
	@./rebar3 skip_deps=true eunit

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
