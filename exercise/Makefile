.PHONY: test clean

OBJS= *.ml

all: test
	./test

test:
	ocamlfind ocamlc -o test -package oUnit -linkpkg -g $(OBJS)

clean:
	rm -f *.cm[io] *.log *.cache test

