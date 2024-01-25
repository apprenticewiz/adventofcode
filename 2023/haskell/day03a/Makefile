GHC = ghc-9.6

TARGET = day03a

all: $(TARGET).exe

$(TARGET).exe: Main.hs
	$(GHC) --make -dynamic -o $@ $<

clean:
	rm -f *.hi *.o *.exe
