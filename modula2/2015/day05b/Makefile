APP = day05b

SRCS = $(APP).mod

M2C = gm2
M2FLAGS = -O2 -march=native -I../../utils

UNIT_OBJS := Args.o CardinalExtras.o DynamicStringFIO.o DynamicStringIO.o

LDFLAGS =

all: $(APP).exe

$(APP).exe: $(SRCS) $(UNIT_OBJS)
	$(M2C) -o $@ $(LDFLAGS) $(M2FLAGS) $^

Args.o: ../../utils/Args.mod
	$(M2C) $(M2FLAGS) -c $<

CardinalExtras.o: ../../utils/CardinalExtras.mod
	$(M2C) $(M2FLAGS) -c $<

DynamicStringFIO.o: ../../utils/DynamicStringFIO.mod
	$(M2C) $(M2FLAGS) -c $<

DynamicStringIO.o: ../../utils/DynamicStringIO.mod
	$(M2C) $(M2FLAGS) -c $<

clean:
	rm -f *.o *.exe
