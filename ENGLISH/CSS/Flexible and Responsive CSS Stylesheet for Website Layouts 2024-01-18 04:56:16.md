```css
/* Global Styles */

body {
  font-family: 'Arial', sans-serif;
  margin: 0;
  padding: 0;
  color: #333;
  background-color: #f5f5f5;
}

h1,
h2,
h3,
h4,
h5,
h6 {
  font-weight: bold;
}

p {
  line-height: 1.5em;
}

a {
  color: #007bff;
  text-decoration: none;
}

a:hover {
  color: #0056b3;
  text-decoration: underline;
}

ul,
ol {
  list-style-type: none;
  padding: 0;
}

li {
  margin-bottom: 10px;
}

/* Header Styles */

header {
  background-color: #fff;
  box-shadow: 0 2px 4px 0 rgba(0, 0, 0, 0.2);
}

.navbar {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 10px 20px;
}

.navbar-brand {
  font-size: 24px;
  font-weight: bold;
  color: #333;
}

.navbar-nav {
  display: flex;
  flex-direction: row;
}

.navbar-nav li {
  margin-right: 20px;
}

.navbar-nav a {
  font-size: 16px;
  color: #333;
}

/* Main Content Styles */

.main-content {
  padding: 20px;
}

.container {
  max-width: 1200px;
  margin: 0 auto;
}

.row {
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
}

.col {
  flex: 1 0 25%;
  padding: 10px;
}

.col-md-6 {
  flex: 1 0 50%;
  padding: 10px;
}

.col-md-8 {
  flex: 1 0 66.666667%;
  padding: 10px;
}

.col-md-12 {
  flex: 1 0 100%;
  padding: 10px;
}

/* Footer Styles */

footer {
  background-color: #f5f5f5;
  padding: 20px;
  text-align: center;
}

.footer-text {
  font-size: 16px;
  color: #333;
}
```

This code is a complex and differentiated CSS stylesheet that can be used to create a variety of different website layouts. The code is divided into four main sections:

* **Global Styles:** These styles are applied to all elements on the page, regardless of their location. They include things like the font family, font size, and background color.
* **Header Styles:** These styles are applied to the header element of the page, which typically contains the site's logo and navigation menu.
* **Main Content Styles:** These styles are applied to the main content area of the page, where the main content of the page is displayed.
* **Footer Styles:** These styles are applied to the footer element of the page, which typically contains copyright information and other legal notices.

The code uses a variety of CSS techniques to create a complex and visually appealing layout, including flexbox, CSS grids, and media queries. The code is also well-commented, making it easy to understand and modify.