```css
/* Reset styles */
* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Base styles */

body {
  font-family: sans-serif;
  background-color: #f5f5f5;
}

h1 {
  font-size: 2em;
  margin-bottom: 10px;
}

h2 {
  font-size: 1.5em;
  margin-bottom: 10px;
}

p {
  font-size: 1em;
  line-height: 1.5em;
  margin-bottom: 10px;
}

a {
  text-decoration: none;
  color: #000;
}

/* Header */

header {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  height: 60px;
  background-color: #fff;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.logo {
  float: left;
  margin-top: 10px;
}

nav {
  float: right;
  margin-top: 10px;
}

nav ul {
  list-style-type: none;
  display: flex;
}

nav li {
  margin-right: 10px;
}

nav a {
  padding: 10px 15px;
  border-radius: 5px;
  background-color: #f5f5f5;
  color: #000;
}

nav a:hover {
  background-color: #eee;
}

/* Main content */

main {
  margin-top: 60px;
  padding: 20px;
}

/* Footer */

footer {
  position: fixed;
  bottom: 0;
  left: 0;
  right: 0;
  height: 60px;
  background-color: #f5f5f5;
  box-shadow: 0 -2px 4px rgba(0, 0, 0, 0.1);
}

.copyright {
  float: left;
  margin-top: 10px;
}

.social-media {
  float: right;
  margin-top: 10px;
}

.social-media ul {
  list-style-type: none;
  display: flex;
}

.social-media li {
  margin-right: 10px;
}

.social-media a {
  padding: 10px 15px;
  border-radius: 5px;
  background-color: #f5f5f5;
  color: #000;
}

.social-media a:hover {
  background-color: #eee;
}

/* Media queries */

@media (max-width: 768px) {
  nav {
    position: absolute;
    top: 60px;
    left: 0;
    right: 0;
    background-color: #fff;
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
  }

  nav ul {
    flex-direction: column;
  }

  nav li {
    margin-bottom: 10px;
  }

  footer {
    position: static;
  }
}
```

This code creates a complex and differentiated CSS layout. It includes a header with a logo and navigation, a main content area, and a footer with a copyright notice and social media links. The code also includes media queries to adjust the layout for smaller screens.

Here is a more detailed explanation of the code:

* The `*` selector resets all elements to their default styles. This is done to ensure that all elements start with the same styles, regardless of their browser defaults.
* The `body` selector sets the font family, background color, and margin and padding to zero.
* The `h1`, `h2`, and `p` selectors set the font size, margin, and line height for headings and paragraphs.
* The `a` selector sets the text decoration and color for links.
* The `header` selector sets the position, top, left, right, height, background color, and box shadow for the header.
* The `.logo` selector sets the float, margin, and image for the logo.
* The `nav` selector sets the float, margin, and padding for the navigation.
* The `nav ul` selector sets the list style type, display, and flex direction for the navigation list.
* The `nav li` selector sets the margin for the navigation list items.
* The `nav a` selector sets the padding, border radius, background color, and color for the navigation links.
* The `nav a:hover` selector sets the background color for the navigation links when they are hovered over.
* The `main` selector sets the margin and padding for the main content area.
* The `footer` selector sets the position, bottom, left, right, height, background color, and box shadow for the footer.
* The `.copyright` selector sets the float, margin, and text for the copyright notice.
* The `.social-media` selector sets the float, margin, and padding for the social media links.
* The `.social-media ul` selector sets the list style type, display, and flex direction for the social media list.
* The `.social-media li` selector sets the margin for the social media list items.
* The `.social-media a` selector sets the padding, border radius, background color, and color for the social media links.
* The `.social-media a:hover` selector sets the background color for the social media links when they are hovered over.
* The `@media (max-width: 768px)` media query adjusts the layout for smaller screens. It changes the position, top, left, right, background color, and box shadow for the navigation, and it changes the flex direction and margin for the navigation list items.