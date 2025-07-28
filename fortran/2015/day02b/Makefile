PROGRAM = day02b

FC      = gfortran
FCFLAGS = -O2 -march=native

$(PROGRAM).exe : $(PROGRAM).o
	$(FC) -o $@ $^

%.o : %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

clean:
	rm -f *.o *.exe *.mod
