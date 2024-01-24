FC = gfortran
FCFLAGS = -O3

TARGET = day03b

all: $(TARGET).exe

$(TARGET).exe: $(TARGET).f03
	$(FC) $(FCFLAGS) -o $@ $<

clean:
	rm -f *.o *.exe *.mod
