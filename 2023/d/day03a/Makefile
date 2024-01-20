TARGET = day03a

DC = gdc

all: $(TARGET).exe

$(TARGET).exe: $(TARGET).d
	$(DC) -O3 -o $@ $<

clean:
	rm -f *.o *.exe
