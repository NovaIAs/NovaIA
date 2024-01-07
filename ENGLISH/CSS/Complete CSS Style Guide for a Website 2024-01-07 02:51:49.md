```css
/* Reset styles */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Body styles */

body {
  font-family: sans-serif;
  font-size: 16px;
  line-height: 1.5;
  color: #333;
}

/* Header styles */

header {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 60px;
  background-color: #fff;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
}

header .logo {
  float: left;
  margin-left: 20px;
}

header .logo img {
  height: 30px;
}

header nav {
  float: right;
  margin-right: 20px;
}

header nav ul {
  list-style: none;
  display: flex;
}

header nav ul li {
  margin-left: 20px;
}

header nav ul li a {
  text-decoration: none;
  color: #333;
}

header nav ul li a:hover {
  color: #000;
}

/* Main content styles */

main {
  margin-top: 60px;
}

main h1 {
  font-size: 2em;
  margin-bottom: 20px;
}

main p {
  font-size: 1.2em;
  line-height: 1.7;
  margin-bottom: 20px;
}

/* Footer styles */

footer {
  position: fixed;
  bottom: 0;
  left: 0;
  width: 100%;
  height: 60px;
  background-color: #f9f9f9;
}

footer .copyright {
  float: left;
  margin-left: 20px;
}

footer .copyright p {
  font-size: 12px;
  color: #666;
}

footer .social-media {
  float: right;
  margin-right: 20px;
}

footer .social-media ul {
  list-style: none;
  display: flex;
}

footer .social-media ul li {
  margin-left: 20px;
}

footer .social-media ul li a {
  text-decoration: none;
  color: #666;
}

footer .social-media ul li a:hover {
  color: #000;
}

/* Media queries */

@media (max-width: 768px) {
  header nav ul {
    display: block;
  }

  header nav ul li {
    float: none;
    margin-left: 0;
  }

  footer .copyright {
    float: none;
    margin-left: 0;
  }

  footer .social-media {
    float: none;
    margin-right: 0;
  }
}
```

This CSS code is a complete style guide for a website. It includes styles for the header, main content, and footer, as well as media queries for responsive design. The code is well-organized and commented, making it easy to understand and modify.

Here is a brief explanation of the code:

* The `*` selector resets all styles to their default values.
* The `body` selector sets the font family, font size, line height, and color for the entire website.
* The `header` selector styles the header section of the website. It includes a logo and a navigation bar.
* The `main` selector styles the main content section of the website. It includes a heading and some paragraphs.
* The `footer` selector styles the footer section of the website. It includes a copyright notice and a social media links section.
* The `@media` rule defines styles for different screen sizes. In this case, the code includes styles for screens that are 768 pixels wide or less.

This code is just a starting point, and you can customize it to fit your specific needs.