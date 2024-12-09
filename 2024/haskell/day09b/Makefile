GHC = ghc

TARGET = day09a

all: $(TARGET).exe

$(TARGET).exe: Main.hs
	$(GHC) -dynamic -o $@ $<

clean:
	rm -f *.hi *.o *.exe
