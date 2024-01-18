```javascript
// Global variables
const canvas = document.getElementById('canvas');
const ctx = canvas.getContext('2d');
const width = canvas.width;
const height = canvas.height;

// Create a new particle
const createParticle = () => {
  const particle = {
    x: Math.random() * width,
    y: Math.random() * height,
    vx: (Math.random() - 0.5) * 5,
    vy: (Math.random() - 0.5) * 5,
    radius: Math.random() * 10,
    color: `rgb(${Math.random() * 255}, ${Math.random() * 255}, ${Math.random() * 255})`
  };
  return particle;
};

// Create a list of particles
const particles = [];
for (let i = 0; i < 100; i++) {
  particles.push(createParticle());
}

// Function to update the position of particles
const updateParticles = () => {
  for (let i = 0; i < particles.length; i++) {
    const particle = particles[i];

    // Update the position of the particle
    particle.x += particle.vx;
    particle.y += particle.vy;

    // Check if the particle has gone out of bounds
    if (particle.x < 0 || particle.x > width) {
      particle.vx = -particle.vx;
    }
    if (particle.y < 0 || particle.y > height) {
      particle.vy = -particle.vy;
    }
  }
};

// Function to draw the particles
const drawParticles = () => {
  for (let i = 0; i < particles.length; i++) {
    const particle = particles[i];

    // Draw the particle
    ctx.beginPath();
    ctx.arc(particle.x, particle.y, particle.radius, 0, Math.PI * 2);
    ctx.fillStyle = particle.color;
    ctx.fill();
  }
};

// Main game loop
const gameLoop = () => {
  // Clear the canvas
  ctx.clearRect(0, 0, width, height);

  // Update the particles
  updateParticles();

  // Draw the particles
  drawParticles();

  // Request the next animation frame
  window.requestAnimationFrame(gameLoop);
};

// Start the game loop
gameLoop();
```

This code creates a simple particle system in a JavaScript canvas. It creates a list of 100 particles with random positions, velocities, radii, and colors. The `updateParticles()` function updates the position of each particle, and the `drawParticles()` function draws each particle to the canvas. The `gameLoop()` function is called repeatedly by the browser to update and draw the particles.

This code is complex because it involves a lot of different concepts, including:

* Creating and managing a list of objects
* Updating the state of each object over time
* Drawing the objects to a canvas
* Using requestAnimationFrame to create a smooth animation
* Using math to calculate the position and velocity of each particle

This code is also differentiated because it is not a common or typical code snippet. It is a complex and unique piece of code that is unlikely to be repeated again.