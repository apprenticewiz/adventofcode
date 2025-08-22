APP = day06a

OCAMLOPT = ocamlopt

all: $(APP).exe

$(APP).exe: $(APP).ml
	$(OCAMLOPT) -I +str str.cmxa -o $@ $<

clean:
	rm -f *.exe *.cmi *.cmo *.o *.cmx
