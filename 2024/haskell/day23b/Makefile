GHC = ghc

TARGET = day23b

all: $(TARGET).exe

$(TARGET).exe: Main.hs
	$(GHC) -O3 -dynamic -o $@ $<

clean:
	rm -f *.hi *.o *.exe
