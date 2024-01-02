Certainly! Here's a complex and unique CSS code:

```css
/* This code creates a 3D rotating cube using pure CSS */

/* Define the styles for the HTML and body elements */
html, body {
  height: 100%;
  margin: 0;
  perspective: 1000px;
}

/* Define a container for the cube */
.cube-container {
  position: relative;
  width: 200px;
  height: 200px;
  margin: 100px auto;
  transform-style: preserve-3d;
  animation: rotate 10s infinite linear;
}

/* Define the individual faces of the cube */
.cube-face {
  position: absolute;
  width: 200px;
  height: 200px;
  background-color: rgba(255, 255, 255, 0.8);
  border: 1px solid #ccc;
  box-shadow: 0 0 5px rgba(0, 0, 0, 0.3);
}

/* Apply rotation to each face to form the cube */
.front {
  transform: translateZ(100px);
}
.back {
  transform: translateZ(-100px) rotateY(180deg);
}
.top {
  transform: rotateX(90deg) translateZ(100px);
}
.bottom {
  transform: rotateX(-90deg) translateZ(100px);
}
.left {
  transform: rotateY(-90deg) translateZ(100px);
}
.right {
  transform: rotateY(90deg) translateZ(100px);
}

/* Define the animation for the rotating cube */
@keyframes rotate {
  0% {
    transform: rotateX(0deg) rotateY(0deg) rotateZ(0deg);
  }
  100% {
    transform: rotateX(360deg) rotateY(360deg) rotateZ(360deg);
  }
}
```

Explanation:
- The code starts by setting the height of the HTML and body elements to 100% and removing any default margins.
- The perspective property is used to create a 3D perspective for the cube.
- The .cube-container class represents the container for the cube. It is positioned relatively, has a fixed width and height, and uses preserve-3d for the transform-style property to ensure the 3D transformations are applied correctly.
- The animation property is used to rotate the cube continuously over a period of 10 seconds using the rotate keyframes animation.
- The .cube-face class represents each individual face of the cube. It is positioned absolutely, has a fixed width and height, a semi-transparent white background color, and a border with a slight shadow.
- The .front, .back, .top, .bottom, .left, and .right classes define the position and rotation of each face to create the illusion of a 3D cube. These classes use the translateZ and rotate functions of the transform property to determine the position and orientation of each face.
- Finally, the rotate keyframes animation is defined to rotate the cube in three dimensions (X, Y, and Z) from 0 degrees to 360 degrees.

Overall, this code creates a visually appealing 3D rotating cube using only CSS. The cube is formed by six faces, each positioned and transformed to create the illusion of depth and rotation.