APP = day05a

OBJS = $(APP).o

CFLAGS = -O2 -march=native

LDFLAGS =

all: $(APP).exe

$(APP).exe: $(OBJS)
	$(CC) -o $@ $(LDFLAGS) $^

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f *.o *.exe
