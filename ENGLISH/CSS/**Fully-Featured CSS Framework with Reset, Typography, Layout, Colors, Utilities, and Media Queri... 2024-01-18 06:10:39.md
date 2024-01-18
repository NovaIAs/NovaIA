```css
/* CSS Reset */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Typography */

body {
  font-family: Arial, sans-serif;
  font-size: 16px;
  line-height: 1.6;
}

h1 {
  font-size: 2.5rem;
  font-weight: bold;
}

h2 {
  font-size: 2rem;
  font-weight: bold;
}

h3 {
  font-size: 1.5rem;
  font-weight: bold;
}

h4 {
  font-size: 1.2rem;
  font-weight: bold;
}

h5 {
  font-size: 1rem;
  font-weight: bold;
}

h6 {
  font-size: 0.8rem;
  font-weight: bold;
}

p {
  margin-bottom: 1rem;
}

a {
  text-decoration: none;
  color: #000;
}

/* Layout */

.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 1rem;
}

.header {
  background-color: #f1f1f1;
  padding: 1rem;
}

.logo {
  float: left;
}

.nav {
  float: right;
}

ul {
  list-style-type: none;
  padding: 0;
}

li {
  display: inline-block;
  margin-right: 1rem;
}

.main {
  margin-top: 1rem;
}

.sidebar {
  float: left;
  width: 20%;
  padding: 1rem;
}

.content {
  float: right;
  width: 80%;
  padding: 1rem;
}

.footer {
  background-color: #f1f1f1;
  padding: 1rem;
}

/* Colors */

:root {
  --primary-color: #007bff;
  --secondary-color: #6c757d;
  --light-color: #f8f9fa;
  --dark-color: #212529;
}

/* Utilities */

.clearfix {
  clear: both;
}

.text-center {
  text-align: center;
}

.text-right {
  text-align: right;
}

.text-muted {
  color: #6c757d;
}

.bg-primary {
  background-color: #007bff;
  color: #fff;
}

.bg-secondary {
  background-color: #6c757d;
  color: #fff;
}

.bg-light {
  background-color: #f8f9fa;
  color: #212529;
}

.bg-dark {
  background-color: #212529;
  color: #fff;
}

/* Media Queries */

@media (max-width: 768px) {
  .sidebar {
    width: 100%;
    float: none;
  }

  .content {
    width: 100%;
    float: none;
  }
}
```

This code is a complex and differentiated CSS code that includes a CSS reset, typography styles, layout styles, colors, utilities, and media queries. The code is organized into sections, making it easy to read and understand. The comments explain the purpose of each section, making it easy to customize the code to your own needs.

The CSS reset section sets all elements to their default styling, ensuring that all browsers render the page consistently. The typography section includes styles for headings, paragraphs, links, and other text elements. The layout section includes styles for the container, header, logo, navigation, main, sidebar, content, and footer. The colors section includes a set of variables that can be used to control the colors of the page. The utilities section includes a number of helper classes that can be used to easily style elements. The media queries section includes styles that are applied to the page when it is viewed on different screen sizes.

This code is just a starting point, and you can customize it to create a website that is unique and tailored to your own needs.