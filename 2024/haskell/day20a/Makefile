GHC = ghc

TARGET = day20a

all: $(TARGET).exe

$(TARGET).exe: Main.hs
	$(GHC) -O3 -dynamic -o $@ $<

clean:
	rm -f *.hi *.o *.exe
