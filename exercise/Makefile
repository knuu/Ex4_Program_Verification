.PHONY: test clean

OBJS= ex_2_*.ml ex_3_*.ml ex_4_*.ml ex_5_*.ml test.ml

all: test
	./test

test:
	ocamlfind ocamlc -o test -package oUnit -linkpkg -g $(OBJS)

clean:
	rm -f *.cmi *.cmo *.log *.cache test

