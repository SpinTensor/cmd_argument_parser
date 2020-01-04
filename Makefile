include Make.inc

all: $(OUTLIB)

$(OUTLIB): cmd_opt_parser.o \
           kinds.o
	ar rvcs $@ $^

cmd_opt_parser.o: cmd_opt_parser.F90 kinds.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -c $<

kinds.o: kinds.F90
	$(FC) $(FCFLAGS) $(FCWFLAGS) -c $<

.PHONY: test buildtest runtest clean distclean

test: $(OUTLIB)
	(cd test; make test)
buildtest: $(OUTLIB)
	(cd test; make buildtest)
runtest: $(OUTLIB)
	(cd test; make runtest)

clean:
	-rm -f *.o *.mod 
	(cd test; make clean)

distclean: clean
	-rm -f $(OUTLIB)
	(cd test; make distclean)
