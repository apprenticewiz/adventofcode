PROGRAM = progname

all: $(PROGRAM).exe

$(PROGRAM).exe: Main.hs
	ghc -o $@ $<

clean:
	rm -f *.hi *.o *.exe
