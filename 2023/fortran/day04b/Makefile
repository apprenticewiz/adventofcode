FC = gfortran
FCFLAGS = -O3

TARGET = day04b

all: $(TARGET).exe

$(TARGET).exe: $(TARGET).f90
	$(FC) $(FCFLAGS) -o $@ $<

clean:
	rm -f *.o *.exe *.mod
