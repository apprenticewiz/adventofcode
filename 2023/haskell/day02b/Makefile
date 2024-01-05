GHC = ghc

TARGET = day02b

all: $(TARGET).exe

$(TARGET).exe: Main.hs
	$(GHC) --make -dynamic -o $@ $<

clean:
	rm -f *.hi *.o *.exe
