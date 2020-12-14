ERLANG_PROJECT=aeserialization

.PHONY: $(ERLANG_PROJECT) transpile stdlib enacl

transpile: $(ERLANG_PROJECT) stdlib enacl
	./erlscripten -p $(ERLANG_PROJECT) -o . --omit aeserialization/_build/default/lib/enacl/ --skip-tests
	spago build

aeserialization:
	cd $(ERLANG_PROJECT); ../rebar3 compile; ../rebar3 eunit

stdlib:
	ln erlps-stdlib/src/* src/ -f
	rm src/Unicode* -f

enacl:
	ln enacl/* src/ -f
