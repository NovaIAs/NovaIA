```
/*
 * Global styles
 */

/* Reset styles */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Base styles */

body {
  font-family: sans-serif;
  line-height: 1.6;
  color: #333;
  background-color: #fff;
}

h1, h2, h3, h4, h5, h6 {
  font-weight: bold;
  line-height: 1.2;
}

p {
  margin-bottom: 1em;
}

a {
  color: #007bff;
  text-decoration: none;

  &:hover {
    color: #0056b3;
    text-decoration: underline;
  }
}

/* Layout */

.container {
  max-width: 1024px;
  padding: 0 1em;
  margin: 0 auto;
}

.row {
  display: flex;
  flex-wrap: wrap;
  margin-right: -1em;
  margin-left: -1em;
}

.col {
  flex: 1 0 auto;
  padding-right: 1em;
  padding-left: 1em;
}

/* Header */

header {
  background-color: #f8f9fa;
  padding: 1em 0;
}

.navbar {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.navbar-brand {
  font-size: 1.5rem;
  font-weight: bold;
}

.navbar-nav {
  display: flex;
  flex-direction: row;
  align-items: center;
}

.navbar-nav li {
  margin-right: 1em;
}

.navbar-nav li a {
  padding: 0.5em 1em;
  border-radius: 0.25rem;
}

/* Main content */

.main {
  padding: 1em 0;
}

.article {
  margin-bottom: 2em;
}

.article-title {
  font-size: 1.5rem;
  font-weight: bold;
  margin-bottom: 0.5em;
}

.article-body {
  line-height: 1.8;
}

/* Footer */

footer {
  background-color: #f8f9fa;
  padding: 1em 0;
}

.footer-copyright {
  font-size: 0.8rem;
  text-align: center;
}

/* Utilities */

.text-center {
  text-align: center;
}

.text-right {
  text-align: right;
}

.text-muted {
  color: #999;
}

.bg-primary {
  background-color: #007bff !important;
}

.bg-secondary {
  background-color: #6c757d !important;
}

.bg-success {
  background-color: #28a745 !important;
}

.bg-info {
  background-color: #17a2b8 !important;
}

.bg-warning {
  background-color: #ffc107 !important;
}

.bg-danger {
  background-color: #dc3545 !important;
}

.border-primary {
  border-color: #007bff !important;
}

.border-secondary {
  border-color: #6c757d !important;
}

.border-success {
  border-color: #28a745 !important;
}

.border-info {
  border-color: #17a2b8 !important;
}

.border-warning {
  border-color: #ffc107 !important;
}

.border-danger {
  border-color: #dc3545 !important;
}

/* Media queries */

@media (max-width: 768px) {
  .navbar-nav {
    flex-direction: column;
    align-items: stretch;
  }

  .navbar-nav li {
    margin-bottom: 1em;
  }
}
```

This code is a complex and differentiated CSS stylesheet that can be used to style a wide variety of web pages. The code is well-commented and organized, making it easy to understand and use.

The code includes styles for:

* Global elements, such as the body, headings, paragraphs, and links
* Layout elements, such as the container, row, and column classes
* Header elements, such as the navbar and logo
* Main content elements, such as articles and blog posts
* Footer elements, such as the copyright notice
* Utility classes, such as text alignment, background colors, and borders
* Media queries for responsive design

This code can be used as a starting point for creating your own CSS stylesheets, or it can be used as a reference when you need to style specific elements on your web pages.