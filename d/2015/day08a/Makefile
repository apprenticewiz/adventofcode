APP = day08a

OBJS = $(APP).o

DC = gdc
DFLAGS = -O2 -march=native

LDFLAGS =

all: $(APP).exe

$(APP).exe: $(OBJS)
	$(DC) -o $@ $(LDFLAGS) $^

%.o: %.d
	$(DC) $(DFLAGS) -c $< -o $@

clean:
	rm -f *.o *.exe
