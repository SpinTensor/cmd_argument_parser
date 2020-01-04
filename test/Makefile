include ../Make.include

CMDOPINC = -I../
CMDOPLIB = -L../ -lcmdopt

all: test

test: runtest

runtest: runtest_get_cmd_arg \
         runtest_get_cmd_opt

buildtest: test_get_cmd_arg.x \
           test_get_cmd_opt.x

runtest_get_cmd_arg: test_get_cmd_arg.x
	@(./test_get_cmd_arg.x 137 42 -17 \
            137.0 42.0 -17.0 \
            "(137.0,1.0)" \
            "(42.0,-1.0)" \
            "(-17.0,3.0)" \
            T ThisIsString )
runtest_get_cmd_opt: test_get_cmd_opt.x
	@(./test_get_cmd_opt.x -is 137 \
            --intd 42 \
            --intq -17 \
            -ias 137 138 139 \
            -iad 42 43 44 \
            -iaq -17 -16 -15 \
            --reals 137.0 \
            --reald 42.0 \
            -rq -17.0 \
            --realarrs 137.0 138.0 139.0 \
            -rad 42.0 43.0 44.0 \
            -raq -17.0 -16.0 -15.0 \
            -cs "(137.0,1.0)" \
            --cmplxd "(42.0,-1.0)" \
            -cq "(-17.0,3.0)" \
            -cas "(137.0,1.0)" "(138.0,1.0)" "(139.0,1.0)" \
            --cmplxarrd "(42.0,-1.0)" "(43.0,-1.0)" "(44.0,-1.0)" \
            --cmplxarrq "(-17.0,3.0)" "(-16.0,3.0)" "(-15.0,3.0)" \
            -l \
            --char "ThisIsString" \
            -ca "ThisIsString1" "ThisIsString2" "ThisIsString3" )

test_get_cmd_arg.x: test_get_cmd_arg.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -o $@ $^ $(CMDOPLIB)
test_get_cmd_arg.o: test_get_cmd_arg.F90 
	$(FC) $(FCFLAGS) $(FCWFLAGS) $(CMDOPINC) -Wno-compare-reals -c $<
test_get_cmd_opt.x: test_get_cmd_opt.o
	$(FC) $(FCFLAGS) $(FCWFLAGS) -o $@ $^ $(CMDOPLIB)
test_get_cmd_opt.o: test_get_cmd_opt.F90
	$(FC) $(FCFLAGS) $(FCWFLAGS) $(CMDOPINC) -Wno-compare-reals -c $<

PHONY: test clean distclean

clean:
	-rm -f *.o *.mod

distclean: clean
	-rm -f *.x
