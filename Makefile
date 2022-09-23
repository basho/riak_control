.PHONY: compile cover test dialyzer
REBAR ?= ./rebar3

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

cover: test
	$(REBAR) cover

test: compile
	$(REBAR) as test do eunit

dialyzer:
	$(REBAR) dialyzer

xref:
	$(REBAR) xref

check: test dialyzer xref

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
