PFUNIT:=/usr/local/pFUnit-3.2.9
FFLAGS:=-gline -C=all -I. -I/usr/local/netcdf-4.6.1/include -I/usr/local/plplot-5.13.0/lib/fortran/modules/plplot
LFLAGS:=-L/usr/local/netcdf-4.6.1/lib -L/usr/local/plplot-5.13.0/lib -L$(PFUNIT)/lib -lpfunit -lplplotfortran -lplplot -lnetcdff -lnetcdf
PFINCL:=-I. -I$(PFUNIT)/mod

main.x: fd1d_heat_explicit.o CFL_mod.o IO_mod.o RHS_mod.o Solver_mod.o Types_mod.o
	nagfor $(LFLAGS) $^ -o $@

pfunit: $(PFUNIT)/include/driver.F90 CFL_mod.o testCFL.o
	nagfor $(PFINCL) $^ $(LFLAGS) -o $@.x
testCFL.o: testCFL.F90 Types_mod.o CFL_mod.o
	nagfor $(FFLAGS) $(PFINCL) -I. -c $<
testCFL.F90: testCFL.pf
	$(PFUNIT)/bin/pFUnitParser.py $< $@ -I.

CFL_mod.o : CFL_mod.f90 Types_mod.o
	nagfor $(FFLAGS) -c $<	
fd1d_heat_explicit.o : fd1d_heat_explicit.f90 CFL_mod.o IO_mod.o RHS_mod.o Solver_mod.o Types_mod.o
	nagfor $(FFLAGS) -c $<
IO_mod.o : IO_mod.f90 Types_mod.o
	nagfor $(FFLAGS) -c $<
RHS_mod.o : RHS_mod.f90 Types_mod.o
	nagfor $(FFLAGS) -c $<
Solver_mod.o : Solver_mod.f90 Types_mod.o RHS_mod.o
	nagfor $(FFLAGS) -c $<
Types_mod.o : Types_mod.f90 
	nagfor $(FFLAGS) -c $<

.PHONY: clean
clean:
	rm -f *.o *.png *.exe *.x *.a

