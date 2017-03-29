lib:
	ocamlbuild -use-ocamlfind -pkgs hardcaml,incremental -tag thread src/sim.cma

.PHONY: test
test:
	ocamlbuild -I src -use-ocamlfind -pkgs hardcaml,ppx_deriving_hardcaml,incremental -tag thread \
		test/test.byte test/cntr.byte

clean:
	ocamlbuild -clean

