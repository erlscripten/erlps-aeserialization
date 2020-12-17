ERLANG_PROJECT=aeserialization

.PHONY: $(ERLANG_PROJECT) transpile stdlib enacl test clean crypto libs

transpile: $(ERLANG_PROJECT) libs
	./erlscripten -p $(ERLANG_PROJECT) -o . --omit aeserialization/_build/default/lib/enacl/ --skip-tests -S "10:base58:b58char" -S "10:base58:charb58" -S "5:aeserialization:decode_field" -S "10:aeser_chain_objects:tag" -S "10:aeser_chain_objects:rev_tag" -S "10:aeser_api_encoder:type2enc" -S "5:aeser_api_encoder:type2pfx" -S "4:aeser_api_encoder:pfx2type"
	spago build

test: $(ERLANG_PROJECT) libs
	./erlscripten -p $(ERLANG_PROJECT) -o . --omit aeserialization/_build/default/lib/enacl/  -S "10:base58:b58char" -S "10:base58:charb58" -S "5:aeserialization:decode_field" -S "10:aeser_chain_objects:tag" -S "10:aeser_chain_objects:rev_tag" -S "10:aeser_api_encoder:type2enc" -S "5:aeser_api_encoder:type2pfx" -S "4:aeser_api_encoder:pfx2type"
	spago test

aeserialization:
	cd $(ERLANG_PROJECT); ../rebar3 compile; ../rebar3 eunit

libs: enacl crypto

enacl:
	ln enacl/* src/ -f

crypto:
	ln crypto/* src/ -f

clean:
	rm src/* -f
	rm test/Aeser* -f
