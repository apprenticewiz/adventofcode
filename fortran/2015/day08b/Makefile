PROGRAM = day08b

FC      = gfortran
FCFLAGS = -O2 -march=native

all : $(PROGRAM).exe

$(PROGRAM).exe : $(PROGRAM).o
	$(FC) -o $@ $^

$(PROGRAM).o : $(PROGRAM).f90
	$(FC) $(FCFLAGS) -o $@ -c $<

clean:
	rm -f *.o *.exe *.mod
