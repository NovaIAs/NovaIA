```css
/* Global Styles */

html, body {
  margin: 0;
  padding: 0;
  font-family: "Helvetica", "Arial", sans-serif;
}

body {
  background-color: #f5f5f5;
}

/* Header Styles */

header {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  height: 60px;
  background-color: #333;
}

header h1 {
  float: left;
  margin: 0;
  padding: 0 20px;
  line-height: 60px;
  font-size: 24px;
  color: #fff;
}

header nav {
  float: right;
  margin: 0;
  padding: 0;
}

header nav ul {
  list-style-type: none;
  margin: 0;
  padding: 0;
}

header nav ul li {
  display: inline-block;
  margin: 0 10px;
}

header nav ul li a {
  text-decoration: none;
  color: #fff;
}

header nav ul li a:hover {
  color: #ccc;
}

/* Main Content Styles */

main {
  margin-top: 60px;
  padding: 20px;
}

main h2 {
  margin-top: 0;
  margin-bottom: 20px;
  font-size: 24px;
}

main p {
  margin: 0;
  font-size: 16px;
}

main ul {
  list-style-type: none;
  padding: 0;
}

main ul li {
  margin-bottom: 10px;
}

/* Footer Styles */

footer {
  position: fixed;
  bottom: 0;
  left: 0;
  right: 0;
  height: 60px;
  background-color: #333;
}

footer p {
  margin: 0;
  padding: 20px;
  color: #fff;
}

/* Media Queries */

@media (max-width: 768px) {
  header nav ul {
    display: none;
  }
}
```

**Explanation:**

This CSS code creates a basic web page layout with a fixed header, main content area, and fixed footer. The header contains a logo and navigation menu, while the main content area contains the main content of the page, such as text, images, and videos. The footer contains a copyright notice or other information.

The code uses a variety of CSS techniques, including:

* **Positioning:** The `position` property is used to position the header and footer at the top and bottom of the page, respectively.
* **Floats:** The `float` property is used to float the logo and navigation menu to the left and right, respectively.
* **Margins and padding:** The `margin` and `padding` properties are used to create space around elements.
* **List styles:** The `list-style-type` property is used to remove the default bullet points from unordered lists.
* **Media queries:** The `@media` rule is used to apply different styles to the page depending on the width of the browser window.

This code is just a starting point, and you can customize it to create your own unique web page layout.