PROGNAME=fd1d_heat_explicit

MODULES=Types_mod RHS_mod CFL_mod IO_mod Solver_mod
MOD_FILES = $(MODULES:=.f90)
OBJ_FILES = $(MOD_FILES:.f90=.o)
MOD_HEADERS = $(MOD_FILES:.f90=.mod)

test : main
	diff h_test01.txt h_test01.txt_bak

main : $(OBJ_FILES) $(PROGNAME).o
	nagfor -I. $^ -o $(PROGNAME).exe

# clear out all suffixes
.SUFFIXES:
# list only those we use
.SUFFIXES: .o .f90

.f90.o :
	nagfor -I. -c $<

.PHONY: clean
clean:
	-rm *.o *.mod
