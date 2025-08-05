APP = day02a

SRCS = $(APP).mod

M2C = gm2
M2FLAGS = -O2 -march=native -fiso

LDFLAGS =

all: $(APP).exe

$(APP).exe: $(SRCS)
	$(M2C) -o $@ $(LDFLAGS) $(M2FLAGS) $^

clean:
	rm -f *.o *.exe
