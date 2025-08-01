APP = day04b

OBJS = $(APP).o md5.o

CFLAGS = -O2 -march=native -I../../utils

LDFLAGS =

all: $(APP).exe

$(APP).exe: $(OBJS)
	$(CC) -o $@ $(LDFLAGS) $^

$(APP).o: $(APP).c
	$(CC) $(CFLAGS) -c $< -o $@

md5.o: ../../utils/md5.c
	$(CC) -I. $(CFLAGS) -c $< -o $@

clean:
	rm -f *.o *.exe
