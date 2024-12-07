GHC = ghc

TARGET = day06b

all: $(TARGET).exe

$(TARGET).exe: Main.hs
	$(GHC) -O2 -dynamic -o $@ $<

clean:
	rm -f *.hi *.o *.exe
