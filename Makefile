lib:
	ocamlbuild -use-ocamlfind -pkgs hardcaml,incremental -tag thread src/sim.cma

.PHONY: test.byte
test.byte:
	ocamlbuild -I src -use-ocamlfind -pkgs hardcaml,ppx_deriving_hardcaml,incremental -tag thread \
		test/test.byte

clean:
	ocamlbuild -clean

