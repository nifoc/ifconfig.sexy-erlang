DEPS = $(CURDIR)/deps

DIALYZER_OPTS = -Wunmatched_returns -Werror_handling -Wrace_conditions

# List dependencies that should be included in a cached dialyzer PLT file.
DIALYZER_DEPS = deps/*/ebin

DEPS_PLT = .ifconfig.plt

ERLANG_DIALYZER_APPS = asn1 \
                       compiler \
                       crypto \
                       erts \
                       inets \
                       kernel \
                       mnesia \
                       public_key \
                       runtime_tools \
                       sasl \
                       ssl \
                       stdlib \
                       syntax_tools \
                       xmerl

all: compile eunit ct dialyzer

# Clean ebin and .eunit of this project
clean:
	@./rebar clean skip_deps=true

# Clean this project and all deps
allclean:
	@./rebar clean

compile: $(DEPS)
	@./rebar compile

$(DEPS):
	@./rebar get-deps

# Full clean and removal of all deps. Remove deps first to avoid
# wasted effort of cleaning deps before nuking them.
distclean:
	@rm -rf deps $(DEPS_PLT)
	@./rebar clean

eunit:
	@./rebar skip_deps=true eunit

ct:
	@./rebar skip_deps=true ct -v

xref:
	@./rebar skip_deps=true xref

test: compile xref eunit ct

# Only include local PLT if we have deps that we are going to analyze
ifeq ($(strip $(DIALYZER_DEPS)),)
dialyzer: ~/.dialyzer_plt
	@dialyzer $(DIALYZER_OPTS) -r ebin
else
dialyzer: ~/.dialyzer_plt $(DEPS_PLT)
	@dialyzer $(DIALYZER_OPTS) --plts ~/.dialyzer_plt $(DEPS_PLT) -r ebin

$(DEPS_PLT):
	@dialyzer --build_plt $(DIALYZER_DEPS) --output_plt $(DEPS_PLT)
endif

~/.dialyzer_plt:
	@echo "ERROR: Missing ~/.dialyzer_plt. Please wait while a new PLT is compiled."
	dialyzer --build_plt --apps $(ERLANG_DIALYZER_APPS)
	@echo "now try your build again"

# Generate documentation
doc:
	@rm -rf ./doc/*.html ./doc/edoc-info ./doc/erlang.png ./doc/stylesheet.css
	@./rebar doc skip_deps=true

release: compile
	@relx

.PHONY: all compile eunit ct xref test dialyzer clean allclean distclean doc release
