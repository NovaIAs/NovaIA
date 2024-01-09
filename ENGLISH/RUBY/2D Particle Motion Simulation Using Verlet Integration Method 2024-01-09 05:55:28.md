```ruby
# This is a program that simulates the movement of a particle in a 2D space.
# The particle is subject to a force field that attracts it to the origin.
# The program uses the Verlet integration method to update the particle's position and velocity.

# Define the mass of the particle.
mass = 1.0

# Define the force constant of the force field.
k = 10.0

# Define the time step.
dt = 0.01

# Initialize the particle's position and velocity.
x = 0.0
y = 0.0
vx = 0.0
vy = 0.0

# Initialize the list of positions.
positions = []

# Simulate the movement of the particle for a number of time steps.
for i in 1..1000
  # Calculate the force acting on the particle.
  fx = -k * x
  fy = -k * y

  # Calculate the particle's acceleration.
  ax = fx / mass
  ay = fy / mass

  # Update the particle's velocity.
  vx += ax * dt
  vy += ay * dt

  # Update the particle's position.
  x += vx * dt + 0.5 * ax * dt^2
  y += vy * dt + 0.5 * ay * dt^2

  # Store the particle's position.
  positions.append([x, y])
end

# Plot the particle's trajectory.
plt.plot(positions)
plt.show()
```

This program simulates the movement of a particle in a 2D space. The particle is subject to a force field that attracts it to the origin. The program uses the Verlet integration method to update the particle's position and velocity.

The program begins by defining the mass of the particle, the force constant of the force field, and the time step. It then initializes the particle's position and velocity.

The program then enters a loop that simulates the movement of the particle for a number of time steps. In each time step, the program calculates the force acting on the particle, the particle's acceleration, and the particle's velocity. The program then updates the particle's position and stores it in a list.

After the loop has finished, the program plots the particle's trajectory.

This program is a good example of how to use the Verlet integration method to simulate the movement of a particle in a 2D space. The program is also a good example of how to use Python to plot data.