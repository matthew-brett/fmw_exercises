PROGNAME=fd1d_heat_explicit

test : main
	diff h_test01.txt h_test01.txt_bak

main : $(PROGNAME).o
	nagfor $^ Types_mod.o RHS_mod.o CFL_mod.o -o $<.exe

$(PROGNAME).o: $(PROGNAME).f90 types_mod.mod rhs_mod.mod cfl_mod.mod
	nagfor -c -I. $< -o $@

types_mod.mod : Types_mod.f90
	nagfor -c $^

rhs_mod.mod : RHS_mod.f90
	nagfor -c $^

cfl_mod.mod : CFL_mod.f90
	nagfor -c $^

.PHONY: clean
clean:
	-rm *.o *.mod
