APP = day03b

OBJS = $(APP).o

DC = gdc
DFLAGS = -O2 -march=native -I../..

LDFLAGS =

all: $(APP).exe

$(APP).exe: $(OBJS)
	$(DC) -o $@ $(LDFLAGS) $^

%.o: %.d
	$(DC) $(DFLAGS) -c $< -o $@

clean:
	rm -f *.o *.exe
