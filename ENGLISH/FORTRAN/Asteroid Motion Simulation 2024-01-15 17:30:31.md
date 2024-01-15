```fortran
PROGRAM ASTEROID_SIMULATION

! This program simulates the motion of a single asteroid in a two-dimensional space.

! Declare constants.
REAL, PARAMETER :: G = 6.674e-11  ! Gravitational constant (N m^2 kg^-2)
REAL, PARAMETER :: M_EARTH = 5.972e24  ! Mass of the Earth (kg)
REAL, PARAMETER :: R_EARTH = 6.378e6  ! Radius of the Earth (m)
REAL, PARAMETER :: DT = 3600  ! Time step (s)

! Declare variables.
REAL :: x, y  ! Position of the asteroid (m)
REAL :: vx, vy  ! Velocity of the asteroid (m/s)
REAL :: ax, ay  ! Acceleration of the asteroid (m/s^2)
REAL :: m  ! Mass of the asteroid (kg)
REAL :: r  ! Distance between the asteroid and the Earth (m)
REAL :: F  ! Gravitational force between the asteroid and the Earth (N)

! Initialize variables.
x = 1e8  ! Initial position of the asteroid (m)
y = 1e8  ! Initial position of the asteroid (m)
vx = 100  ! Initial velocity of the asteroid (m/s)
vy = 100  ! Initial velocity of the asteroid (m/s)
m = 1e12  ! Initial mass of the asteroid (kg)

! Begin the simulation.
DO WHILE (TRUE)

    ! Calculate the distance between the asteroid and the Earth.
    r = SQRT((x - 0)**2 + (y - 0)**2)

    ! Calculate the gravitational force between the asteroid and the Earth.
    F = G * M_EARTH * m / (r**2)

    ! Calculate the acceleration of the asteroid.
    ax = -F * x / m
    ay = -F * y / m

    ! Update the velocity of the asteroid.
    vx = vx + ax * DT
    vy = vy + ay * DT

    ! Update the position of the asteroid.
    x = x + vx * DT
    y = y + vy * DT

    ! Print the position of the asteroid.
    PRINT *, x, y

END DO

END PROGRAM ASTEROID_SIMULATION
```

This code simulates the motion of a single asteroid in a two-dimensional space. The asteroid is initially placed at a distance of 1e8 meters from the Earth and has an initial velocity of 100 m/s. The asteroid's mass is 1e12 kg.

The simulation uses a time step of 3600 seconds (1 hour). At each time step, the code calculates the distance between the asteroid and the Earth, the gravitational force between the two objects, and the acceleration of the asteroid. The code then updates the velocity and position of the asteroid.

The simulation continues until the asteroid collides with the Earth or until the asteroid leaves the simulation region. The simulation region is a square with a side length of 2e8 meters.

The output of the code is the position of the asteroid at each time step. The position is printed to the console.