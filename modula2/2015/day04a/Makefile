APP = day04a

SRCS = $(APP).mod

M2C = gm2
M2FLAGS = -O2 -march=native -I../../utils

UNIT_OBJS := Args.o DynamicStringIO.o MD5.o

LDFLAGS =

all: $(APP).exe

$(APP).exe: $(SRCS) $(UNIT_OBJS)
	$(M2C) -o $@ $(LDFLAGS) $(M2FLAGS) $^

Args.o: ../../utils/Args.mod
	$(M2C) $(M2FLAGS) -c $<

DynamicStringIO.o: ../../utils/DynamicStringIO.mod
	$(M2C) $(M2FLAGS) -c $<

MD5.o: ../../utils/MD5.mod
	$(M2C) $(M2FLAGS) -c $<

clean:
	rm -f *.o *.exe
