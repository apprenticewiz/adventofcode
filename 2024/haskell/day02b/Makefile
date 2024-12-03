GHC = ghc

TARGET = day02b

all: $(TARGET).exe

$(TARGET).exe: Main.hs
	$(GHC) -dynamic -o $@ $<

clean:
	rm -f *.hi *.o *.exe
