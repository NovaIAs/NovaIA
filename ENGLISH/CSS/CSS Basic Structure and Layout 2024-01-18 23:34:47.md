```CSS
/* Universal selector */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Body styles */

body {
  font-family: sans-serif;
  background-color: #f5f5f5;
}

/* Header styles */

header {
  height: 60px;
  background-color: #333;
  color: #fff;
  display: flex;
  justify-content: center;
  align-items: center;
}

/* Logo styles */

header .logo {
  font-size: 24px;
  font-weight: bold;
}

/* Navigation styles */

nav {
  display: flex;
  justify-content: center;
  align-items: center;
}

nav ul {
  display: flex;
  list-style: none;
}

nav li {
  margin-right: 20px;
}

nav a {
  text-decoration: none;
  color: #fff;
}

/* Main content styles */

main {
  margin-top: 60px;
}

/* Article styles */

article {
  margin-bottom: 30px;
}

article h2 {
  font-size: 24px;
  font-weight: bold;
}

article p {
  font-size: 16px;
}

/* Footer styles */

footer {
  height: 60px;
  background-color: #333;
  color: #fff;
  display: flex;
  justify-content: center;
  align-items: center;
}

/* Copyright styles */

footer .copyright {
  font-size: 12px;
}

/* Media queries */

@media (max-width: 768px) {
  nav ul {
    flex-direction: column;
  }

  nav li {
    margin-bottom: 10px;
  }
}
```

Explanation:

* The universal selector `*` applies styles to all elements on the page.
* The `body` selector styles the body element.
* The `header` selector styles the header element.
* The `.logo` class selector styles the element with the `.logo` class.
* The `nav` selector styles the navigation element.
* The `ul` selector styles the unordered list element.
* The `li` selector styles the list item element.
* The `a` selector styles the anchor element.
* The `main` selector styles the main content element.
* The `article` selector styles the article element.
* The `h2` selector styles the heading 2 element.
* The `p` selector styles the paragraph element.
* The `footer` selector styles the footer element.
* The `.copyright` class selector styles the element with the `.copyright` class.
* The `@media (max-width: 768px)` media query applies styles to screens with a maximum width of 768 pixels.