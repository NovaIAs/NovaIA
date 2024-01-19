/*

Welcome to the realm of CSS complexity! This code is designed to showcase a wide range of CSS techniques, resulting in a highly elaborate and unique design. Let's dive into the details:
*/

/*
1. Base Styling:
*/

html,
body {
  font-family: 'Helvetica', sans-serif;
  background-color: #f2f2f2;
  margin: 0;
  padding: 0;
}

/*
2. Container Div:
*/

div.container {
  width: 100vw;
  height: 100vh;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
}

/*
3. Gradient Background:
*/

div.container:before {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: linear-gradient(45deg, #ff9a9e 0%, #fad0c4 99%, #fad0c4 100%);
  z-index: -1;
}

/*
4. Animated Text:
*/

h1 {
  font-size: 10rem;
  color: #05386b;
  text-align: center;
  animation: text-anim 5s infinite alternate;
}

@keyframes text-anim {
  0% {
    color: #05386b;
    text-shadow: 0 0 10px #ff9a9e, 0 0 20px #fad0c4, 0 0 30px #ff9a9e, 0 0 40px #fad0c4;
  }
  50% {
    color: #ff9a9e;
    text-shadow: 0 0 10px #05386b, 0 0 20px #fad0c4, 0 0 30px #05386b, 0 0 40px #fad0c4;
  }
  100% {
    color: #fad0c4;
    text-shadow: 0 0 10px #05386b, 0 0 20px #ff9a9e, 0 0 30px #05386b, 0 0 40px #ff9a9e;
  }
}

/*
5. Rotating Boxes:
*/

div.boxes {
  display: flex;
  justify-content: space-around;
  align-items: center;
  width: 100%;
  height: 50%;
}

div.box {
  width: 150px;
  height: 150px;
  background-color: #05386b;
  border-radius: 50%;
  margin: 20px;
  transform: rotate(45deg);
  animation: box-anim 10s infinite alternate;
}

@keyframes box-anim {
  0% {
    transform: rotate(45deg);
    box-shadow: 0 0 10px #ff9a9e, 0 0 20px #fad0c4, 0 0 30px #ff9a9e, 0 0 40px #fad0c4;
  }
  50% {
    transform: rotate(-45deg);
    box-shadow: 0 0 10px #05386b, 0 0 20px #fad0c4, 0 0 30px #05386b, 0 0 40px #fad0c4;
  }
  100% {
    transform: rotate(45deg);
    box-shadow: 0 0 10px #05386b, 0 0 20px #ff9a9e, 0 0 30px #05386b, 0 0 40px #ff9a9e;
  }
}

/*
6. Particle Animation:
*/

div.particles {
  position: absolute;
  left: 0;
  top: 0;
  width: 100vw;
  height: 100vh;
  pointer-events: none;
}

div.particle {
  position: absolute;
  width: 5px;
  height: 5px;
  border-radius: 50%;
  background-color: #ff9a9e;
  animation: particle-anim 2s infinite alternate;
}

@keyframes particle-anim {
  0% {
    transform: translate(0, 0);
    opacity: 1;
  }
  50% {
    transform: translate(100vw, 100vh);
    opacity: 0.5;
  }
  100% {
    transform: translate(0, 0);
    opacity: 1;
  }
}

/*
7. Media Queries:
*/

@media (max-width: 992px) {
  h1 {
    font-size: 8rem;
  }

  div.boxes {
    flex-direction: column;
    height: 70%;
  }

  div.box {
    margin: 10px;
  }
}

/*
Explanation:
*/

- Base Styling: We establish the basic styles for HTML and the body, including font and background.

- Container Div: This div is the main container, centered vertically and horizontally.

- Gradient Background: The before element creates a linear gradient background with animations.

- Animated Text: The large, centered text has an animation that changes color and shadow hues continuously.

- Rotating Boxes: Multiple rotating boxes with shadows. The animation alternates their rotation and shadow colors.

- Particle Animation: Tiny particles move randomly across the screen, creating a dynamic effect.

- Media Queries: The code adapts to smaller screen sizes, adjusting the layout and font size.

Overall, this complex CSS code showcases a wide range of techniques, making it a visually captivating and intricate demonstration of CSS possibilities.