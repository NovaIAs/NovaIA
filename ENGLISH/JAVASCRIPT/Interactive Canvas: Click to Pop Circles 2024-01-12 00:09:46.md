```javascript
(function() {
  // Define the main function
  const main = () => {
    // Create a function to generate a random number between two values
    const randomNumber = (min, max) => {
      return Math.floor(Math.random() * (max - min + 1)) + min;
    };

    // Define an array of colors
    const colors = ['red', 'orange', 'yellow', 'green', 'blue', 'indigo', 'violet'];

    // Define an array to store the circles
    const circles = [];

    // Create 10 circles
    for (let i = 0; i < 10; i++) {
      // Generate a random position for the circle
      const x = randomNumber(0, window.innerWidth);
      const y = randomNumber(0, window.innerHeight);

      // Generate a random radius for the circle
      const radius = randomNumber(10, 50);

      // Generate a random color for the circle
      const color = colors[randomNumber(0, colors.length - 1)];

      // Create a new circle object
      const circle = {
        x,
        y,
        radius,
        color,
      };

      // Add the circle to the array
      circles.push(circle);
    }

    // Define a function to draw a circle
    const drawCircle = (circle) => {
      const canvas = document.getElementById('canvas');
      const ctx = canvas.getContext('2d');

      ctx.beginPath();
      ctx.arc(circle.x, circle.y, circle.radius, 0, 2 * Math.PI);
      ctx.fillStyle = circle.color;
      ctx.fill();
    };

    // Draw all the circles
    circles.forEach(drawCircle);

    // Add an event listener for mouse clicks
    document.addEventListener('click', (event) => {
      // Get the position of the mouse click
      const x = event.clientX;
      const y = event.clientY;

      // Check if any of the circles were clicked
      for (let i = 0; i < circles.length; i++) {
        const circle = circles[i];

        // Calculate the distance between the mouse click and the center of the circle
        const dx = x - circle.x;
        const dy = y - circle.y;
        const distance = Math.sqrt(dx * dx + dy * dy);

        // If the distance is less than the radius of the circle, then the circle was clicked
        if (distance < circle.radius) {
          // Remove the circle from the array
          circles.splice(i, 1);

          // Redraw all the circles
          circles.forEach(drawCircle);

          // Break out of the loop
          break;
        }
      }
    });
  };

  // Call the main function
  main();
})();
```

**Explanation:**

This code creates a canvas element and draws 10 circles on it. The circles are randomly positioned, have random radii, and random colors. When the user clicks on a circle, it is removed from the canvas.

Here is a breakdown of the code:

* The `main` function is the main entry point for the program. It creates a function to generate random numbers, an array of colors, and an array to store the circles.
* The `randomNumber` function generates a random number between two values.
* The `colors` array contains the colors that can be used for the circles.
* The `circles` array stores the circles that are drawn on the canvas.
* The `drawCircle` function draws a circle on the canvas.
* The `document.addEventListener('click', ...)` event listener listens for mouse clicks on the canvas.
* When the user clicks on the canvas, the `event` object is passed to the event handler. The event handler gets the position of the mouse click and checks if any of the circles were clicked.
* If a circle was clicked, it is removed from the `circles` array and the remaining circles are redrawn.

This code is a bit more complex than the previous examples, but it demonstrates how to use JavaScript to create interactive graphics.