EXEC=mlinterp

CMOFILES= \
	Value.cmo \
	Context.cmo \
	State.cmo \
	ValueUtils.cmo \
	Interpreter.cmo \
	Main.cmo

%.cmo: %.ml
	ocamlfind ocamlc -o $@ -package batteries -package compiler-libs.common -c $<

$(EXEC): $(CMOFILES)
	ocamlfind ocamlc -o $@ -package batteries -package compiler-libs.common -linkpkg $^

all: $(EXEC)
clean:
	rm *.cmo
	rm *.cmi
	rm $(EXEC)
