```css
/* Global styles */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
  font-family: 'Helvetica', 'Arial', sans-serif;
}

body {
  background-color: #f5f5f5;
  color: #333;
  font-size: 16px;
  line-height: 1.5;
}

/* Header styles */

header {
  background-color: #fff;
  padding: 20px;
  box-shadow: 0px 2px 5px rgba(0, 0, 0, 0.1);
}

header h1 {
  font-size: 24px;
  font-weight: bold;
}

header nav {
  float: right;
}

header nav ul {
  list-style: none;
  display: flex;
}

header nav li {
  margin-right: 20px;
}

header nav a {
  text-decoration: none;
  color: #333;
}

header nav a:hover {
  color: #000;
}

/* Main content styles */

main {
  padding: 20px;
}

main h2 {
  font-size: 20px;
  font-weight: bold;
}

main p {
  margin-bottom: 15px;
}

main ul {
  list-style: none;
  padding: 0;
}

main li {
  margin-bottom: 10px;
}

main a {
  text-decoration: none;
  color: #000;
}

main a:hover {
  color: #f00;
}

/* Footer styles */

footer {
  background-color: #f5f5f5;
  padding: 20px;
  text-align: center;
}

footer p {
  font-size: 14px;
  color: #666;
}

/* Media queries */

@media (max-width: 768px) {
  header nav {
    float: none;
    width: 100%;
    text-align: center;
  }

  header nav ul {
    flex-direction: column;
  }

  header nav li {
    margin-right: 0;
    margin-bottom: 10px;
  }
}
```

This is a complex CSS code that includes a variety of styles for different elements on a web page. The code is well-commented and easy to understand, making it a good example of how to write complex CSS.

The code starts with some global styles that are applied to all elements on the page. These styles include the font family, font size, and line height.

The next section of the code styles the header element. The header has a white background color, a padding of 20px, and a box shadow. The header also contains an h1 element for the page title and a nav element for the navigation links.

The nav element is styled with a float: right; property, which makes it float to the right side of the header. The nav element also contains an ul element for the navigation links. The ul element is styled with a display: flex; property, which makes the navigation links display horizontally.

The next section of the code styles the main element. The main element is the main content area of the page and contains the h2 headings, p paragraphs, and ul lists. The h2 headings are styled with a font size of 20px and a font weight of bold. The p paragraphs are styled with a margin-bottom of 15px. The ul lists are styled with a padding of 0 and a margin-bottom of 10px.

The next section of the code styles the footer element. The footer element is located at the bottom of the page and contains a p element for the copyright information. The p element is styled with a font size of 14px and a color of #666.

The final section of the code contains some media queries that apply different styles to the page when the screen width is less than 768px. These styles include making the navigation links display vertically instead of horizontally.

Overall, this is a complex CSS code that includes a variety of styles for different elements on a web page. The code is well-commented and easy to understand, making it a good example of how to write complex CSS.