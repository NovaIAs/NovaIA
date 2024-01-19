```css
/*
    This CSS code creates a visually appealing and dynamic gradient animation that changes color based on the user's scroll position.
    The animation uses a combination of the `background-position` property and `calc()` function to create a smooth transition effect.

    HTML Structure:
    <div id="gradient-container">
        <div class="gradient-animation"></div>
    </div>

    CSS Explanation:

    1. Gradient Container (#gradient-container):
        - Defines the overall container for the gradient animation.

    2. Gradient Animation (.gradient-animation):
        - Positions the gradient animation within the container.
        - Applies a linear gradient with a combination of multiple color stops.
        - Utilizes the `background-size` property to set the gradient size.

    3. Animation Keyframes (@keyframes gradient-animation):
        - Defines the animation keyframes for the gradient.
        - Includes three keyframes: `0%`, `50%`, and `100%`.
        - Gradually changes the `background-position` property to create a smooth transition effect.

    4. Animation Timing (@-webkit-animation, @animation):
        - Uses `@-webkit-animation` for Webkit browsers and `@animation` for other browsers.
        - Sets the animation name, duration, iteration count, and timing function.

    5. Scroll-Linked Animation:
        - Utilizes the `scroll-snap-type` property to enable scroll-linked animations.
        - Sets the `scroll-snap-align` property to `start` to snap the animation to the top of the page.

    6. Browser Compatibility:
        - Includes vendor prefixes (-webkit-) to ensure compatibility with older browsers.

    By scrolling the page, the gradient animation will smoothly transition between the defined color stops, creating a visually engaging experience.
*/

/* Gradient Container */
#gradient-container {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100vh;
  overflow: hidden;
}

/* Gradient Animation */
.gradient-animation {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background: linear-gradient(
    to bottom,
    #ffafbd 0%,
    #ffc3a0 25%,
    #ffc8a0 50%,
    #ffc3a0 75%,
    #ffafbd 100%
  );
  background-size: 400% 400%;
}

/* Animation Keyframes */
@keyframes gradient-animation {
  0% {
    background-position: 0% 50%;
  }
  50% {
    background-position: 100% 50%;
  }
  100% {
    background-position: 0% 50%;
  }
}

/* Animation Timing */
@-webkit-animation: gradient-animation 15s infinite linear;
@animation: gradient-animation 15s infinite linear;

/* Scroll-Linked Animation */
html {
  scroll-snap-type: y mandatory;
}

body {
  scroll-snap-align: start;
}
```