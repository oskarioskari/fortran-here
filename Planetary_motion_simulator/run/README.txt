Before running check first README.txt in
/src for compilation instructions.

 - - -

For complete instructions on how to
initialize input.dat, read report.
Input file input.dat must be filled
in following way:

> objects
> [integer number n]
> masses
> [mass of object 1 ]
> [mass of object 2 ]
> [ ...             ]
> [mass of object n ]
> positions
> [position of object 1]
> [position of object 2]
> [ ...                ]
> [position of object n]
> velocities
> [velocity of object 1]
> [velocity of object 2]
> [ ...                ]
> [velocity of object n]
> print
> [integer number]
> write
> [integer number]
> simulation
> lenght
> [real number]
> timestep
> [real number]
> steps
> [integer number]
> end

 - - -

Position lines should be formatted as:
> [x-coordinate] [y-coordinate] [z-coordinate]

Velocity lines should be formatted as:
> [x-component] [y-component] [z-component]

 - - -

Masses must be given in kilograms.
Coordinates are given in meters.
Velocities are given in meters per second.
Simulation length and time-step in seconds.

 - - -

Folder /run must also contain file named output.dat.
If needed, create empty file with that name.

When .f90 files are compiled, input.dat is
initialized and output.dat is present, program
can be ran by executing simulation.exe in /src:

> ./simulation.exe


