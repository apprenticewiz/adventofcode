APP = day01b

SRCS = $(APP).mod

M2C = gm2
M2FLAGS = -O2 -march=native -I../../utils

ARGS_OBJ = Args.o

LDFLAGS =

all: $(APP).exe

$(APP).exe: $(SRCS) $(ARGS_OBJ)
	$(M2C) -o $@ $(LDFLAGS) $(M2FLAGS) $^

$(ARGS_OBJ): ../../utils/Args.mod
	$(M2C) -c $(M2FLAGS) $<

clean:
	rm -f *.o *.exe
