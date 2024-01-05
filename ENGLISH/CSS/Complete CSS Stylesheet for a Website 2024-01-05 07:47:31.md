```css
/* CSS Reset */

*{
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Global Styles */

body {
  font-family: 'Arial', sans-serif;
  font-size: 16px;
  line-height: 1.6;
  color: #333;
  background-color: #fff;
}

h1, h2, h3, h4, h5, h6 {
  font-family: 'Georgia', serif;
  font-weight: bold;
  margin-bottom: 1.2em;
}

p {
  margin-bottom: 1.2em;
}

a {
  color: #007aff;
  text-decoration: none;
}

a:hover {
  color: #005fa3;
}

/* Header */

header {
  background-color: #f1f1f1;
  padding: 1em;
}

header h1 {
  font-size: 2.4em;
}

/* Navigation */

nav {
  background-color: #333;
  padding: 1em;
}

nav ul {
  list-style-type: none;
  display: flex;
}

nav li {
  margin-right: 1em;
}

nav a {
  color: #fff;
}

/* Main Content */

main {
  padding: 1em;
}

/* Footer */

footer {
  background-color: #f1f1f1;
  padding: 1em;
}

footer p {
  font-size: 0.8em;
}

/* Media Queries */

@media (max-width: 768px) {
  nav ul {
    flex-direction: column;
  }

  nav li {
    margin-bottom: 1em;
  }
}
```

This code is a complete CSS stylesheet for a website. It includes a CSS reset, global styles, styles for the header, navigation, main content, footer, and media queries.

The CSS reset is important because it normalizes the styles of all elements on the page. This ensures that the website will look consistent across all browsers.

The global styles define the font family, font size, line height, and color of the text on the page. They also define the background color of the page.

The header styles define the background color, padding, and font size of the header.

The navigation styles define the background color, padding, and list style type of the navigation. They also define the margin and color of the navigation links.

The main content styles define the padding of the main content area.

The footer styles define the background color, padding, and font size of the footer.

The media queries define how the website will look on different screen sizes. In this case, the media query defines how the navigation will look on screens that are 768px wide or less.