PROGRAM = day04a

CC      = gcc
FC      = gfortran

CFLAGS  = -O2 -march=native
FCFLAGS = -O2 -march=native

$(PROGRAM).exe : $(PROGRAM).o md5_module.o md5.o
	$(FC) -o $@ $^

$(PROGRAM).o : $(PROGRAM).f90 md5_module.o
	$(FC) $(FCFLAGS) -c $< -o $@

md5_module.o : ../../utils/md5/md5_module.f90
	$(FC) $(FCFLAGS) -c $< -o $@

md5.o : ../../utils/md5/md5.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f *.o *.exe *.mod
