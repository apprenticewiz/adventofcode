PROGRAM = day03b

CFLAGS = -O2 -march=native

$(PROGRAM).exe : $(PROGRAM).ali
	gnatbind -x $<
	gnatlink $<
	mv $(PROGRAM) $(PROGRAM).exe

%.ali : %.adb
	gcc $(CFLAGS) -c $<

clean:
	rm -f *.o *.ali *.exe
