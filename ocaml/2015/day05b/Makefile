APP = day05b

OCAMLOPT = ocamlopt

all: $(APP).exe

$(APP).exe: $(APP).ml
	$(OCAMLOPT) -o $@ $<

clean:
	rm -f *.exe *.cmi *.cmo *.o *.cmx
