```css
/* CSS Code */

/* General Styles */

html, body {
  height: 100%;
  margin: 0;
  padding: 0;
  font-family: sans-serif;
}

/* Header Styles */

header {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 60px;
  background-color: #333;
  color: #fff;
  text-align: center;
}

header h1 {
  line-height: 60px;
  font-size: 24px;
  font-weight: bold;
}

/* Navigation Styles */

nav {
  position: fixed;
  top: 60px;
  left: 0;
  width: 100%;
  height: 40px;
  background-color: #444;
  color: #fff;
}

nav ul {
  list-style-type: none;
  display: flex;
  justify-content: space-around;
  align-items: center;
}

nav li {
  padding: 0 10px;
}

nav a {
  color: #fff;
  text-decoration: none;
}

nav a:hover {
  color: #ddd;
}

/* Main Content Styles */

main {
  position: relative;
  top: 100px;
  left: 0;
  width: 100%;
}

main section {
  padding: 20px;
}

main h2 {
  font-size: 30px;
  font-weight: bold;
  margin-bottom: 10px;
}

main p {
  font-size: 16px;
  line-height: 1.5em;
}

/* Footer Styles */

footer {
  position: fixed;
  bottom: 0;
  left: 0;
  width: 100%;
  height: 40px;
  background-color: #333;
  color: #fff;
  text-align: center;
}

footer p {
  line-height: 40px;
  font-size: 14px;
}

/* Additional Styles */

.container {
  max-width: 1200px;
  margin: 0 auto;
}

.btn {
  display: inline-block;
  padding: 8px 16px;
  border: 1px solid #444;
  border-radius: 4px;
  background-color: #444;
  color: #fff;
  text-decoration: none;
}

.btn:hover {
  background-color: #333;
}

.hide {
  display: none;
}

@media screen and (max-width: 768px) {
  nav {
    position: relative;
    top: 0;
    left: 0;
    width: 100%;
    height: auto;
  }

  nav ul {
    flex-direction: column;
  }

  nav li {
    padding: 10px 0;
  }
}
```

**Explanation:**

This CSS code creates a complex and well-structured web page layout with a header, navigation, main content, and footer. It includes styles for general elements like the `html` and `body`, as well as specific styles for each section of the page.

The header has a fixed position at the top of the page, a black background, and white text. It contains a centered `<h1>` element with the site's title.

The navigation bar is also fixed and positioned below the header. It has a black background and white text. It contains an unordered list (`<ul>`) of links, styled using flexbox to be evenly spaced and centered horizontally.

The main content area takes up the remaining space below the navigation bar. It contains multiple sections, each with a `<h2>` heading and paragraphs of text.

The footer is fixed at the bottom of the page, has a black background, and white text. It contains a simple paragraph with the site's copyright information.

In addition to these basic styles, the code also includes styles for buttons, a hide class to hide elements, and media queries to adjust the layout for smaller screens.

Overall, this code demonstrates a variety of CSS techniques and is suitable for creating a visually appealing and functional web page layout.