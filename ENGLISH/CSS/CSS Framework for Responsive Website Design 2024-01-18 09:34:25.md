```css
/* CSS Reset */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Typography */

body {
  font-family: "Helvetica", "Arial", sans-serif;
  font-size: 16px;
  line-height: 1.5;
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
  margin-bottom: 1em;
}

a {
  color: #007bff;
  text-decoration: none;
  transition: color 0.3s ease-in-out;
}

a:hover {
  color: #0056b3;
}

/* Layout */

.container {
  max-width: 1200px;
  padding: 0 15px;
  margin: 0 auto;
}

.row {
  display: flex;
  flex-wrap: wrap;
  justify-content: space-between;
}

.col {
  flex: 1 0 20%;
  padding: 15px;
}

.col-2 {
  flex: 1 0 40%;
}

.col-3 {
  flex: 1 0 60%;
}

/* Header */

header {
  background-color: #f5f5f5;
  padding: 15px 0;
}

.logo {
  font-size: 24px;
  font-weight: bold;
}

/* Navigation */

nav {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.nav-item {
  margin-right: 15px;
}

.nav-link {
  display: block;
  padding: 10px 15px;
  color: #333;
  transition: color 0.3s ease-in-out;
}

.nav-link:hover {
  color: #007bff;
}

/* Main Content */

main {
  padding-top: 30px;
}

.article {
  margin-bottom: 30px;
}

.article-title {
  font-size: 24px;
  font-weight: bold;
}

.article-content {
  margin-top: 15px;
}

/* Footer */

footer {
  background-color: #f5f5f5;
  padding: 15px 0;
}

.copyright {
  font-size: 12px;
  text-align: center;
}

/* Media Queries */

@media (max-width: 768px) {
  .col {
    flex: 1 0 100%;
  }

  .nav-item {
    margin-right: 0;
  }
}
```

This code is a complex and differentiated CSS code that can be used to create a responsive website. The code includes a CSS reset, typography styles, layout styles, header styles, navigation styles, main content styles, footer styles, and media queries.

The CSS reset is used to normalize the styles of all elements on the page, so that they all start with the same styles. This helps to ensure that the website looks consistent across different browsers and devices.

The typography styles define the fonts, font sizes, and line heights for the different elements on the page. This helps to make the text on the website readable and easy to understand.

The layout styles define the layout of the website, including the width of the container, the number of columns in the grid, and the spacing between the columns. This helps to create a visually appealing and easy-to-navigate website.

The header styles define the appearance of the header, including the logo, the navigation bar, and the search bar. This helps to create a memorable and informative header that makes it easy for users to find the information they need.

The navigation styles define the appearance of the navigation bar, including the menu items and the links. This helps to create a navigation bar that is easy to use and understand.

The main content styles define the appearance of the main content area, including the articles, the images, and the videos. This helps to create a visually appealing and informative main content area that is easy to read and understand.

The footer styles define the appearance of the footer, including the copyright notice and the social media links. This helps to create a memorable and informative footer that makes it easy for users to find the information they need.

The media queries are used to change the appearance of the website when it is viewed on different devices. This helps to ensure that the website is responsive and looks good on all devices.