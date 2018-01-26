program main
  use constants
  use definitions
  implicit none

  integer :: i,j,ios,objects,m,k,steps,stepsFromStart,written
  real(rk) :: x,timestep,length,a,b,c
  real(rk),dimension(3) :: temp
  real(rk),allocatable :: masses(:)
  type(vector) :: speed,testvector
  type(vector),allocatable :: velocities(:),velocitiesTemp(:)
  type(vector),allocatable :: forces(:),forcesTemp(:),forcesOnObject(:),accelerations(:),accelerationsTemp(:)
  type(position) :: place
  type(position),allocatable :: coordinates(:),coordinatesTemp(:)
  character(len=80) :: text
  character(len=80) :: inputfile='../run/input.dat'
  character(len=80) :: outputfile='../run/output.dat'
  character(len=80) :: plotter='python plot.py'
  
  ! Open file
  ! Read headers from file
  ! Save values into variables depending on headers
  ! In case of error print error message specifying cause of error

  open(unit=1, file=inputfile, iostat=ios, status='old')
  if (ios .ne. 0) THEN
     print *,'Error in opening file ',inputfile
     stop
  end if
  
  read_inputfile: do
     read(1,*, iostat=ios) text

     ! If header is 'objects' then read number of objects and
     ! allocate arrays
     if (text .eq. 'objects') THEN
        read(1,*,iostat=ios) objects
        if (ios .ne. 0) THEN
           print *,'Error while reading objects from ',inputfile
           stop
        end if
        allocate(masses(objects))
        allocate(velocities(objects), velocitiesTemp(objects))
        allocate(coordinates(objects), coordinatesTemp(objects))
        allocate(forces(objects), forcesTemp(objects), forcesOnObject(objects))
        allocate(accelerations(objects), accelerationsTemp(objects))

     ! If header is 'masses' save values into 'masses'
     else if (text .eq. 'masses') THEN
        do j=1,objects
           read(1,*,iostat=ios) x
           if (ios .ne. 0) THEN
              print *,'Error while reading masses from ',inputfile
              stop
           end if
           masses(j)=x
        end do

     ! If header is 'velocities' save values into 'velocities'
     else if (text .eq. 'velocities') THEN
        do j=1,objects
           read(1,*,iostat=ios) temp
           if (ios .ne. 0) THEN
              print *,'Error while reading velocities from ',inputfile
              stop
           end if
           speed=vector(temp(1),temp(2),temp(3))
           velocities(j)=speed
        end do

     ! If header is 'positions' save values into 'positions'
     else if (text .eq. 'positions') THEN
        do j=1,objects
           read(1,*,iostat=ios) temp
           if (ios .ne. 0) THEN
              print *,'Error while reading coordinates from input.dat'
              stop
           end if
           place=position(temp(1),temp(2),temp(3))
           coordinates(j)=place
        end do

     ! If header is 'simulation' save values into 'timestep',
     ! 'lenght' and 'steps' according to headers.
     else if (text .eq. 'simulation') THEN
        do j=1,3
           read(1,*,iostat=ios) text
           if (ios .ne. 0) THEN
              print *,'Error while reading simulation from ',inputfile
              stop
           end if

           ! Check if next header is 'timestep', 'length' or 'steps'
           if (text .eq. 'timestep') THEN
              read(1,*,iostat=ios) timestep
              if (ios .ne. 0) THEN
                 print *,'Error while reading value of timestep from ',inputfile
                 stop
              end if
           else if (text .eq. 'lenght') THEN
              read(1,*,iostat=ios) length
              if (ios .ne. 0) THEN
                 print *,'Error while reading value of lenght from ',inputfile
                 stop
              end if
           else if (text .eq. 'steps') THEN
              read(1,*,iostat=ios) steps
              if (ios .ne. 0) THEN
                 print *,'Error while reading number of steps from ',inputfile
                 stop
              end if
           end if
        end do

     ! Print every 'm' steps
     else if (text .eq. 'print') THEN
        read(1,*,iostat=ios) m
        if (ios .ne. 0) THEN
           print *,'Error while reading value for m in print'
           stop
        end if
     
     ! Write every 'k' steps to output.dat
     else if (text .eq. 'write') THEN
        read(1,*,iostat=ios) k
        if (ios .ne. 0) THEN
           print *,'Error while reading value for k in write'
           stop
        end if

     ! Exit do loop if text == end
     ! You can add text after end-line if needed and not to worry about
     ! it affecting run of program.
     else if (text .eq. 'end') THEN
        exit
     end if
     
     if (ios<0) exit
  end do read_inputfile
  
  close(1)
  
  ! **************************************************************************************************************************************
  ! Missing values for step '0' are calculated here.
  
  ! Set origin to center of mass
  coordinates=setCenterOfMass(masses,objects,coordinates)

  ! Check if steps, length or timestep equals 0.
  ! Calculate missing value
  if (steps .eq. 0) THEN
     steps=nint(length/timestep)
  else if (length .eq. 0) THEN
     length=timestep*steps
  else if (timestep .eq. 0) THEN
     timestep=length/steps
  end if
  
  ! Calculate forces at start
  start_values_forces: do i=1,objects

     ! If calculating forces for first object in array:
     if (i .eq. 1) THEN
        do j=2,objects
           forcesOnObject(j)=gravity(coordinates(j),coordinates(i),masses(j),masses(i))
        end do
        forces(i)=vector(0,0,0)
        do j=2,objects
           a=forces(i)%x+forcesOnObject(j)%x
           b=forces(i)%y+forcesOnObject(j)%y
           c=forces(i)%z+forcesOnObject(j)%z
           forces(i)=vector(a,b,c)
        end do

     ! If calculating forces for last object in array:
     else if (i .eq. objects) THEN
        do j=1,objects-1
           forcesOnObject(j)=gravity(coordinates(j),coordinates(i),masses(j),masses(i))
        end do
        forces(i)=vector(0,0,0)
        do j=1,objects-1
           a=forces(i)%x+forcesOnObject(j)%x
           b=forces(i)%y+forcesOnObject(j)%y
           c=forces(i)%z+forcesOnObject(j)%z
           forces(i)=vector(a,b,c)
        end do

     ! For every other object:
     else
        do j=1,i-1
           forcesOnObject(j)=gravity(coordinates(j),coordinates(i),masses(j),masses(i))
        end do
        do j=i+1,objects
           forcesOnObject(j-1)=gravity(coordinates(j),coordinates(i),masses(j),masses(i))
        end do
        forces(i)=vector(0,0,0)
        do j=1,objects-1
           a=forces(i)%x+forcesOnObject(j)%x
           b=forces(i)%y+forcesOnObject(j)%y
           c=forces(i)%z+forcesOnObject(j)%z
           forces(i)=vector(a,b,c)
        end do
     end if
  end do start_values_forces
  
  ! Calculate accelerations at start
  start_values_acceleration: do i=1,objects
     accelerations(i)=acceleration(masses(i),forces(i))
  end do start_values_acceleration
  
  ! **************************************************************************************************************************************
  ! Simulation
  
  stepsFromStart=0
  
  ! Open output-file
  open(unit=2, file=outputfile, iostat=ios, status='old')
  if (ios .ne. 0) THEN
     print *,'Error in opening file ',outputfile
     stop
  end if

  ! Write starting values to output-file
  write (2,*) 'Objects:',objects
  write (2,*) 'Number_of_steps: ',stepsFromStart
  write (2,*) 'Time_from_start_(s): ',stepsFromStart*timestep
  write (2,*) 'Positions_of_objects: '
  do i=1,objects
     write (2,*) 'Object',i,coordinates(i)%x,coordinates(i)%y,coordinates(i)%z
  end do
  write (2,*) 'Velocities_of_objects:'
  do i=1,objects
     write (2,*) 'Object',i,velocities(i)%x,velocities(i)%y,velocities(i)%z
  end do
  write (2,*) 'Forces_on_objects:'
  do i=1,objects
     write (2,*) 'Object',i,forces(i)%x,forces(i)%y,forces(i)%z
  end do
  write (2,*) ''
  written=1

  ! Print starting values
  print *,'Objects: ',objects
  print *,'Number of steps: ',stepsFromStart
  print *,'Time from start (s): ',stepsFromStart*timestep
  print *,'Steps written in file: ',written
  print *,'Positions of objects: '
  do i=1,objects
     print '(a7,i2,a,3f20.1)','Object ',i,':',coordinatesTemp(i)%x,coordinatesTemp(i)%y,coordinatesTemp(i)%z
  end do
  
  ! Simulation loop
  Iteration: do

     positions: do i=1,objects
        coordinatesTemp(i)=newPosition(coordinates(i),velocities(i),timestep,accelerations(i))
     end do positions
     
     ! Set origin to center of mass:
     coordinatesTemp=setCenterOfMass(masses, objects, coordinatesTemp)
     
     new_forces: do i=1,objects
        
        ! If calculating forces for first object in array:
        if (i .eq. 1) THEN
           do j=2,objects
              forcesOnObject(j)=gravity(coordinatesTemp(j),coordinatesTemp(i),masses(j),masses(i))
           end do
           forcesTemp(i)=vector(0,0,0)
           do j=2,objects
              a=forcesTemp(i)%x+forcesOnObject(j)%x
              b=forcesTemp(i)%y+forcesOnObject(j)%y
              c=forcesTemp(i)%z+forcesOnObject(j)%z
              forcesTemp(i)=vector(a,b,c)
           end do
           
           ! If calculating forces for last object in array:
        else if (i .eq. objects) THEN
           do j=1,objects-1
              forcesOnObject(j)=gravity(coordinatesTemp(j),coordinatesTemp(i),masses(j),masses(i))
           end do
           forcesTemp(i)=vector(0,0,0)
           do j=1,objects-1
              a=forcesTemp(i)%x+forcesOnObject(j)%x
              b=forcesTemp(i)%y+forcesOnObject(j)%y
              c=forcesTemp(i)%z+forcesOnObject(j)%z
              forcesTemp(i)=vector(a,b,c)
           end do
           
           ! For every other object:
        else
           do j=1,i-1
              forcesOnObject(j)=gravity(coordinatesTemp(j),coordinatesTemp(i),masses(j),masses(i))
           end do
           do j=i+1,objects
              forcesOnObject(j-1)=gravity(coordinatesTemp(j),coordinatesTemp(i),masses(j),masses(i))
           end do
           forcesTemp(i)=vector(0,0,0)
           do j=1,objects-1
              a=forcesTemp(i)%x+forcesOnObject(j)%x
              b=forcesTemp(i)%y+forcesOnObject(j)%y
              c=forcesTemp(i)%z+forcesOnObject(j)%z
              forcesTemp(i)=vector(a,b,c)
           end do
        end if
     end do new_forces
     
     new_accelerations: do i=1,objects
        accelerationsTemp(i)=acceleration(masses(i),forcesTemp(i))
     end do new_accelerations
     
     new_velocities: do i=1,objects
        velocitiesTemp(i)=newVelocity(velocities(i),accelerations(i),accelerationsTemp(i),timestep)
     end do new_velocities
     
     
     ! Write to file and/or print to command line if conditions are met.

     stepsFromStart=stepsFromStart+1

     ! If number of steps is divisible by k, write to file:
     if (mod(stepsFromStart,k) == 0 .OR. stepsFromStart >= steps) THEN
        write (2,*) 'Number_of_steps: ',stepsFromStart
        write (2,*) 'Time_from_start_(s): ',stepsFromStart*timestep
        write (2,*) 'Positions_of_objects:'
        do i=1,objects
           write (2,*) 'Object',i,coordinatesTemp(i)%x,coordinatesTemp(i)%y,coordinatesTemp(i)%z
        end do
        write (2,*) 'Velocities_of_objects:'
        do i=1,objects
           write (2,*) 'Object',i,velocitiesTemp(i)%x,velocitiesTemp(i)%y,velocitiesTemp(i)%z
        end do
        write (2,*) 'Forces_on_objects:'
        do i=1,objects
           write (2,*) 'Object',i,forcesTemp(i)%x,forcesTemp(i)%y,forcesTemp(i)%z
        end do
        write (2,*) ''
        written=written+1
     end if
     
     ! If number of steps is divisible by m, print:
     if (mod(stepsFromStart,m) == 0 .OR. stepsFromStart >= steps) THEN
        print *,'Objects: ',objects
        print *,'Number of steps: ',stepsFromStart
        print *,'Time from start (s): ',stepsFromStart*timestep
        print *,'Steps written in file: ',written
        print *,'Positions of objects: '
        do i=1,objects
           print '(a7,i2,a,3f20.1)','Object ',i,':',coordinatesTemp(i)%x,coordinatesTemp(i)%y,coordinatesTemp(i)%z
        end do
     end if
     
     ! Exit if stepsFromStart >= steps, else prepare for new round
     if (stepsFromStart .ge. steps) THEN
        close(2)
        exit
     else
        replace_old_values: do i=1,objects
           coordinates(i)=coordinatesTemp(i)
           velocities(i)=velocitiesTemp(i)
           forces(i)=forcesTemp(i)
           accelerations(i)=accelerationsTemp(i)
        end do replace_old_values
     end if

  end do Iteration

  ! Run plot.py
  CALL EXECUTE_COMMAND_LINE(plotter, WAIT=.TRUE.)
  
  stop

  ! **************************************************************************************************************************************
  ! Functions

contains

  ! Calculates gravity on object at position p2 caused by object at position p1
  type(vector) function gravity(p1,p2,m1,m2)
    implicit none
    type(position),intent(in) :: p1,p2
    real(rk),intent(in) :: m1,m2
    type(vector) :: F1,p12,p12unit
    real(rk) :: G,p12l,F,E
    G=6.67259*0.00000000001

    ! Calculate vector from p1 to p2 and its length
    p12=vector((p2%x-p1%x), (p2%y-p1%y), (p2%z-p1%z))
    p12l=sqrt(((p12%x)**2)+((p12%y)**2)+((p12%z)**2))
    p12unit=vector(p12%x/p12l, p12%y/p12l, p12%z/p12l)
    
    ! Force
    F=-G*((m1*m2)/(p12l**2))
    F1=vector(F*p12unit%x, F*p12unit%y, F*p12unit%z)
    
    gravity=F1
    return
  end function gravity

  ! Calculate acceleration when force in known
  type(vector) function acceleration(m, f)
    implicit none
    real(rk),intent(in) :: m
    type(vector),intent(in) :: f
    acceleration=vector(f%x/m, f%y/m, f%z/m)
    return
  end function acceleration
  
  ! Calculate next position of object
  type(position) function newPosition(p0,v0,dt,a0)
    implicit none
    type(position),intent(in) :: p0
    type(position) :: p1
    type(vector),intent(in) :: v0,a0
    real(rk),intent(in) :: dt
    real(rk) :: x1,y1,z1
    x1=p0%x+(v0%x*dt)+(0.5*a0%x*dt**2)
    y1=p0%y+(v0%y*dt)+(0.5*a0%y*dt**2)
    z1=p0%z+(v0%z*dt)+(0.5*a0%z*dt**2)
    p1=position(x1,y1,z1)
    newPosition=p1
    return
  end function newPosition
  
  ! Calculate next velocity of object
  type(vector) function newVelocity(v0,a0,a1,dt)
    implicit none
    type(vector),intent(in) :: v0,a0,a1
    type(vector) :: v1
    real(rk),intent(in) :: dt
    real(rk) :: vx,vy,vz
    vx=v0%x+0.5*(a0%x+a1%x)*dt
    vy=v0%y+0.5*(a0%y+a1%y)*dt
    vz=v0%z+0.5*(a0%z+a1%z)*dt
    v1=vector(vx,vy,vz)
    newVelocity=v1
  end function newVelocity

  ! Calculate center of mass and set origin to it, function returns a list of positions
  ! mass_list = list holding masses of objects
  ! quantity  = number of objects
  ! current_positions = list of coordinates
  function setCenterOfMass(mass_list, quantity, current_positions)
    implicit none
    integer,intent(in) :: quantity
    integer :: q
    type(position),intent(in) :: current_positions(:)
    type(position) :: new_positions(quantity), weighted_positions(quantity), center
    type(position),dimension(quantity) :: setCenterOfMass
    real(rk),intent(in) :: mass_list(:)
    real(rk) :: total_mass, i, j, k
    total_mass=0.0
    do q=1,quantity
       total_mass=total_mass+masses(q)
    end do
    do q=1,quantity
       i=mass_list(q)*current_positions(q)%x
       j=mass_list(q)*current_positions(q)%y
       k=mass_list(q)*current_positions(q)%z
       weighted_positions(q)=position(i,j,k)
    end do
    center=position(0,0,0)
    do q=1,quantity
       i=center%x+(1/total_mass)*weighted_positions(q)%x
       j=center%y+(1/total_mass)*weighted_positions(q)%y
       k=center%z+(1/total_mass)*weighted_positions(q)%z
       center=position(i,j,k)
    end do
    do q=1,quantity
       i=current_positions(q)%x-center%x
       j=current_positions(q)%y-center%y
       k=current_positions(q)%z-center%z
       new_positions(q)=position(i,j,k)
    end do
    setCenterOfMass=new_positions
  end function setCenterOfMass
  
end program main
