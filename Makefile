TARGET = a.out
FC = gfortran
DEBUG = yes

ifeq ($(FC),gfortran)
  ifeq ($(DEBUG),yes)
    FFLAGS := -std=f95 -Wall -Wuninitialized -O0 -g3 -fbounds-check \
              -fbacktrace -fdump-core -ffpe-trap=invalid,zero,overflow -fimplicit-none \
              -finit-real=snan -finit-integer=-858993460
  else
    FFLAGS := -std=f95 -O2 -march=native -fbacktrace -fdump-core
  endif
else ifeq ($(FC),ifort)
  ifeq ($(DEBUG),yes)
    FFLAGS = -std95 -g -O0 -assume byterecl -warn -implicitnone -mkl=sequential \
             -traceback -C -fpe0
  else
    FFLAGS = -std95 -O2 -march=native -assume byterecl -mkl -traceback
  endif
endif

$(TARGET): main.f90 Makefile
	$(FC) $(FFLAGS) $< -o $@

clean:
	rm -rf *.o *.mod *.log $(TARGET) core.*

.PHONY: clean debug

