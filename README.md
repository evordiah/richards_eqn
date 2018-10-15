# richards_eqn
Richards equation from Gottardi / Venutelli (1993)

small, personal university project to port functions to other programming languages of choice
- c++
- R

compiling fortran 77 files using gfortran:

example program, save as hiworld.f:

        program hello
            print *, "Hello World!"
        end program hello

I installed MinGW to get the gnu utils, including gfortran. At command line in the same directory as hiworld.f run:

    gfortran hiworld.f -o hi

This will run the compiler and create an output executable file called "hi.exe" (since I'm on Windows). Run that and - if you see your cheery message - you can compile fortran.

