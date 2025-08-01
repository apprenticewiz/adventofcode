APP = day03a

OBJS = $(APP).o string_set.o

CFLAGS = -O2 -march=native -I../../utils

LDFLAGS =

all: $(APP).exe

$(APP).exe: $(OBJS)
	$(CC) -o $@ $(LDFLAGS) $^

$(APP).o: $(APP).c
	$(CC) $(CFLAGS) -c $< -o $@

string_set.o: ../../utils/string_set.c
	$(CC) -I. $(CFLAGS) -c $< -o $@

clean:
	rm -f *.o *.exe
