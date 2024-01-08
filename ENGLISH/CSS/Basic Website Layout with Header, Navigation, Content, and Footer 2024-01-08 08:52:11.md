```css
/* Global Styles */

body {
  margin: 0;
  padding: 0;
  font-family: 'Helvetica', 'Arial', sans-serif;
}

/* Header Styles */

header {
  width: 100%;
  height: 100px;
  background-color: #333;
  color: #fff;
}

header h1 {
  font-size: 36px;
  line-height: 100px;
  text-align: center;
}

/* Navigation Styles */

nav {
  width: 100%;
  height: 50px;
  background-color: #555;
  color: #fff;
}

nav ul {
  list-style-type: none;
  display: flex;
  justify-content: center;
  align-items: center;
}

nav li {
  margin: 0 10px;
}

nav a {
  color: #fff;
  text-decoration: none;
}

nav a:hover {
  color: #ddd;
}

/* Content Styles */

main {
  width: 100%;
  padding: 50px;
}

main h2 {
  font-size: 24px;
  margin-bottom: 20px;
}

main p {
  font-size: 16px;
  line-height: 1.5em;
}

/* Footer Styles */

footer {
  width: 100%;
  height: 50px;
  background-color: #333;
  color: #fff;
}

footer p {
  text-align: center;
  line-height: 50px;
}

/* Responsive Styles */

@media (max-width: 768px) {
  header {
    height: 60px;
  }

  header h1 {
    font-size: 24px;
    line-height: 60px;
  }

  nav {
    height: 40px;
  }

  nav ul {
    flex-direction: column;
    align-items: flex-start;
  }

  nav li {
    margin: 10px 0;
  }

  main {
    padding: 20px;
  }

  main h2 {
    font-size: 20px;
  }

  main p {
    font-size: 14px;
  }

  footer {
    height: 40px;
  }

  footer p {
    line-height: 40px;
  }
}
```

This code creates a basic website layout with a header, navigation, content, and footer. The code is written in CSS, which is a style sheet language used to style HTML elements.

The `body` element is the root element of the HTML document, and the `margin` and `padding` properties are set to 0 to remove any default spacing around the element. The `font-family` property is set to 'Helvetica', 'Arial', sans-serif', which specifies the font that will be used for the text on the page.

The `header` element is the header of the website, and the `width`, `height`, `background-color`, and `color` properties are set to create a header that is 100 pixels high, has a black background, and white text. The `h1` element inside the header is the heading for the website, and the `font-size`, `line-height`, and `text-align` properties are set to create a heading that is 36 pixels high, centered, and has a line height of 100 pixels.

The `nav` element is the navigation bar for the website, and the `width`, `height`, `background-color`, and `color` properties are set to create a navigation bar that is 50 pixels high, has a gray background, and white text. The `ul` element inside the navigation bar is the list of links, and the `list-style-type` property is set to 'none' to remove the default bullet points from the list. The `display` property is set to 'flex' to create a flexbox layout, and the `justify-content` and `align-items` properties are set to center the links horizontally and vertically. The `li` elements inside the `ul` element are the individual links, and the `margin` property is set to create a 10 pixel margin between each link. The `a` elements inside the `li` elements are the actual links, and the `color` and `text-decoration` properties are set to create links that are white and have no underline. The `a:hover` selector is used to style the links when they are hovered over, and the `color` property is set to change the color of the links to light gray.

The `main` element is the main content area of the website, and the `width` and `padding` properties are set to create a content area that is 100 pixels wide and has 50 pixels of padding on all sides. The `h2` elements inside the main element are the headings for the content sections, and the `font-size` and `margin-bottom` properties are set to create headings that are 24 pixels high and have a 20 pixel margin below them. The `p` elements inside the main element are the paragraphs of text, and the `font-size` and `line-height` properties are set to create paragraphs that are 16 pixels high and have a line height of 1.5em.

The `footer` element is the footer of the website, and the `width`, `height`, `background-color`, and `color` properties are set to create a footer that is 50 pixels high, has a black background, and white text. The `p` element inside the footer is the copyright notice, and the `text-align` and `line-height` properties are set to center the text and make it vertically centered within the footer.

The `@media` rule is used to create responsive styles for the website, which means that the styles will change depending on the size of the screen. The `@media (max-width: 768px)` rule applies to screens that are 768 pixels wide or less, and the styles inside the rule will be applied to those screens. The styles inside the `@media` rule change the layout of the website to make it more suitable for smaller screens. For example, the header and navigation bar are made smaller, the content area is made narrower, and the font sizes are reduced.