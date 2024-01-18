```css
/* Reset Styles */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

html,
body {
  height: 100%;
  font-family: sans-serif;
}

/* Main Content */

main {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  text-align: center;
  height: 100vh;
}

h1 {
  font-size: 3rem;
  margin-bottom: 1rem;
}

p {
  font-size: 1.2rem;
  line-height: 1.5rem;
  margin-bottom: 2rem;
}

/* Button Styles */

.button {
  display: inline-block;
  padding: 0.5rem 1rem;
  border: 2px solid #000;
  border-radius: 5px;
  text-decoration: none;
  font-size: 1rem;
  font-weight: bold;
  color: #000;
  background-color: transparent;
  transition: all 0.2s ease-in-out;
}

.button:hover {
  background-color: #000;
  color: #fff;
}

/* Additional Styles */

.visually-hidden {
  display: none;
}

/* Media Queries */

@media (max-width: 768px) {
  h1 {
    font-size: 2rem;
  }

  p {
    font-size: 1rem;
    line-height: 1.2rem;
  }

  .button {
    font-size: 0.8rem;
  }
}
```

**Explanation:**

* The `*` selector resets all elements' styles to their defaults.
* The `html` and `body` elements are set to 100% height to fill the entire viewport.
* The `main` element is the main content area and is centered both vertically and horizontally.
* The `h1` and `p` elements are styled for the main content text.
* The `.button` class styles the button element. It is initially transparent, but when hovered over, it turns black with white text.
* The `.visually-hidden` class is used to hide elements from the visual display.
* The media query at the bottom of the code changes the styles of certain elements for devices with a maximum width of 768px.

This code demonstrates a combination of various CSS techniques, including reset styles, flexbox layout, media queries, and hover effects.