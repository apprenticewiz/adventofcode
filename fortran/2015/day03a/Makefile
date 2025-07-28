PROGRAM = day03a

FC      = gfortran
FCFLAGS = -O2 -march=native

$(PROGRAM).exe : $(PROGRAM).o string_set.o
	$(FC) -o $@ $^

string_set.o : ../../utils/string_set/string_set.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

day03a.o : day03a.f90 string_set.o
	$(FC) $(FCFLAGS) -o $@ -c $<

clean:
	rm -f *.o *.exe *.mod
