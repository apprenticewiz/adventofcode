FC = gfortran
FCFLAGS = -std=f2003

TARGET = day01b

all: $(TARGET).exe

$(TARGET).exe: $(TARGET).f03
	$(FC) $(FCFLAGS) -o $@ $<

clean:
	rm -f *.o *.exe
