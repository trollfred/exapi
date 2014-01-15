.PHONY: deps docs

all: check-deps
	@./rebar compile

deps:
	@./rebar get-deps

check-deps:
	@./rebar check-deps

app.config:
	@cp app.config.orig app.config

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@./rebar skip_deps=true doc

test: test-eunit test-ct

test-eunit:
	@./rebar eunit skip_deps=true

test-ct:
	@./rebar ct skip_deps=true verbose=1

PLT_NAME=.exapi_dialyzer.plt

build-plt:
	@ERL_LIBS=deps dialyzer --build_plt --output_plt $(PLT_NAME) \
	--apps erts kernel stdlib sasl crypto xmlrpc || true

dialyze: $(PLT_NAME)
	@dialyzer ebin --plt $(PLT_NAME) --no_native \
	-Werror_handling -Wunderspecs

NODE_NAME=exapi@localhost
COOKIE=exapi

run: check-deps
	ERL_LIBS=deps:apps erl -sname $(NODE_NAME) -setcookie $(COOKIE) -boot start_sasl +P 2000000 -s exapi-dev

console:
	@( erl -pa apps/*/ebin deps/*/ebin deps/*/include ebin )

REM_NODE_NAME="exapi_remsh_${shell echo $$$$}@localhost"

attach:
	erl -sname $(REM_NODE_NAME) -remsh $(NODE_NAME) -setcookie $(COOKIE)
