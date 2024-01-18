```css
/* Overriding browser defaults */
* {
  font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
  font-weight: 400;
  line-height: 1.6;
  color: #333;
}

/* Base styles for the page */
body {
  background-color: #eee;
  margin: 0;
  padding: 0;
}

/* Header styles */
header {
  background-color: #333;
  color: #fff;
  padding: 1em;
}

header h1 {
  font-size: 2em;
  margin-bottom: 0.5em;
}

header p {
  font-size: 1.2em;
}

/* Navigation styles */
nav {
  background-color: #eee;
  padding: 1em;
}

nav ul {
  list-style-type: none;
  display: flex;
  justify-content: space-around;
}

nav li {
  display: inline-block;
  padding: 0.5em;
}

nav a {
  text-decoration: none;
  color: #333;
}

/* Main content styles */
main {
  padding: 1em;
}

main h2 {
  font-size: 1.5em;
  margin-bottom: 0.5em;
}

main p {
  font-size: 1.2em;
}

/* Footer styles */
footer {
  background-color: #333;
  color: #fff;
  padding: 1em;
}

footer p {
  font-size: 1.2em;
}

/* Media query for small screens */
@media (max-width: 768px) {
  nav ul {
    flex-direction: column;
  }
}
```

Explanation:

* The code starts by overriding the default styles for all elements in the page.
* The `body` element is given a background color, and the margins and padding are set to 0.
* The `header` element is given a background color, a color for the text, and a padding of 1em.
* The `h1` element inside the `header` is given a font size of 2em and a margin-bottom of 0.5em.
* The `p` element inside the `header` is given a font size of 1.2em.
* The `nav` element is given a background color, a color for the text, and a padding of 1em.
* The `ul` element inside the `nav` is given a display property of `flex`, a `justify-content` property of `space-around`, and a `list-style-type` property of `none`.
* The `li` elements inside the `ul` are given a display property of `inline-block` and a padding of 0.5em.
* The `a` elements inside the `li` elements are given a text-decoration property of `none` and a color of #333.
* The `main` element is given a padding of 1em.
* The `h2` elements inside the `main` element are given a font size of 1.5em and a margin-bottom of 0.5em.
* The `p` elements inside the `main` element are given a font size of 1.2em.
* The `footer` element is given a background color, a color for the text, and a padding of 1em.
* The `p` element inside the `footer` is given a font size of 1.2em.
* The code ends with a media query for small screens, which changes the layout of the navigation bar when the screen width is less than 768px.