Certainly! Here's a complex and unique CSS code. This code will create a visually appealing and interactive 3D rotating cube. Let me explain the code step by step:

```css
/* HTML */
html,
body {
  height: 100%;
}

body {
  display: flex;
  justify-content: center;
  align-items: center;
  perspective: 1000px; /* Adds perspective to the 3D space */
}

.scene {
  width: 200px;
  height: 200px;
  position: relative;
  transform-style: preserve-3d; /* Enables 3D transformations */
  animation: rotate 10s infinite linear; /* Animation to rotate the cube */
}

.cube {
  width: 100%;
  height: 100%;
  position: absolute;
  transform-style: preserve-3d; /* Enables 3D transformations */
}

.face {
  width: 200px;
  height: 200px;
  position: absolute;
  border: 2px solid black;
}

/* Set different colors for each face */
.front {
  background-color: coral;
  transform: translateZ(100px); /* Moves the face towards the viewer */
}

.back {
  background-color: dodgerblue;
  transform: translateZ(-100px) rotateY(180deg); /* Moves the face away from the viewer and rotates it 180 degrees */
}

.left {
  background-color: limegreen;
  transform: rotateY(-90deg) translateX(-100px); /* Rotates the face to the left and moves it to the left side of the cube */
}

.right {
  background-color: purple;
  transform: rotateY(90deg) translateX(100px); /* Rotates the face to the right and moves it to the right side of the cube */
}

.top {
  background-color: gold;
  transform: rotateX(90deg) translateY(-100px); /* Rotates the face upwards and moves it to the top of the cube */
}

.bottom {
  background-color: tomato;
  transform: rotateX(-90deg) translateY(100px); /* Rotates the face downwards and moves it to the bottom of the cube */
}

/* Animation to rotate the cube */
@keyframes rotate {
  0% {
    transform: rotateX(0) rotateY(0) rotateZ(0);
  }
  
  100% {
    transform: rotateX(360deg) rotateY(360deg) rotateZ(360deg);
  }
}
```

Explanation:

1. The HTML and body elements are set to a height of 100% to cover the entire viewport.

2. The body is flex container with center alignment, allowing the cube to be centered in the page.

3. The `.scene` class represents the 3D space in which the cube will be rendered. It is given a width and height of 200px and a perspective of 1000px, adding depth to the cube.

4. The `.cube` class represents the cube itself. It is absolutely positioned within the `.scene` and has the `transform-style` property set to `preserve-3d` to enable 3D transformations.

5. The `.face` class represents each face of the cube. Each face has a width and height of 200px and a black border.

6. The `.front`, `.back`, `.left`, `.right`, `.top`, and `.bottom` classes represent each face of the cube. Each face has a different background color and `transform` property to position it correctly in 3D space.

7. The `@keyframes rotate` rule defines an animation that rotates the cube 360 degrees around all three axes. This animation is then applied to the `.scene` class with a duration of 10 seconds, making the cube continuously rotate.

When you incorporate this CSS code into an HTML file, you will see a 3D rotating cube with different colored faces. Feel free to modify the colors or dimensions to suit your needs.