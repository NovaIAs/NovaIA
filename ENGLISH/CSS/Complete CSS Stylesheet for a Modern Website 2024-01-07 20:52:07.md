```css
/* Reset styles */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Typography */

body {
  font-family: "Helvetica", sans-serif;
  font-size: 16px;
  line-height: 1.5;
  color: #333;
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
  margin-bottom: 1rem;
}

a {
  text-decoration: none;
  color: #007bff;
}

a:hover {
  color: #0056b3;
}

/* Layout */

.container {
  max-width: 1024px;
  padding: 0 1rem;
  margin: 0 auto;
}

.row {
  display: flex;
  flex-wrap: wrap;
}

.col {
  flex: 1;
  padding: 0 1rem;
}

.col-2 {
  flex: 2;
}

.col-3 {
  flex: 3;
}

.col-4 {
  flex: 4;
}

/* Header */

.header {
  background-color: #f8f9fa;
  padding: 1rem;
}

.header-logo {
  font-size: 2rem;
  font-weight: bold;
}

/* Navigation */

.nav {
  list-style-type: none;
  display: flex;
  justify-content: space-between;
}

.nav-item {
  padding: 0.5rem 1rem;
}

.nav-link {
  color: #333;
}

.nav-link:hover {
  color: #007bff;
}

/* Main content */

.main {
  padding: 1rem;
}

.main-title {
  font-size: 2rem;
  font-weight: bold;
  margin-bottom: 1rem;
}

.main-content {
  line-height: 1.5;
  margin-bottom: 1rem;
}

/* Footer */

.footer {
  background-color: #f8f9fa;
  padding: 1rem;
  text-align: center;
}

/* Utilities */

.text-center {
  text-align: center;
}

.text-right {
  text-align: right;
}

.text-bold {
  font-weight: bold;
}

.text-italic {
  font-style: italic;
}

.text-uppercase {
  text-transform: uppercase;
}

.text-lowercase {
  text-transform: lowercase;
}

.bg-primary {
  background-color: #007bff;
}

.bg-secondary {
  background-color: #6c757d;
}

.bg-success {
  background-color: #28a745;
}

.bg-danger {
  background-color: #dc3545;
}

.bg-warning {
  background-color: #ffc107;
}

.bg-info {
  background-color: #17a2b8;
}

.bg-light {
  background-color: #f8f9fa;
}

.bg-dark {
  background-color: #343a40;
}

.border {
  border: 1px solid #000;
}

.border-top {
  border-top: 1px solid #000;
}

.border-right {
  border-right: 1px solid #000;
}

.border-bottom {
  border-bottom: 1px solid #000;
}

.border-left {
  border-left: 1px solid #000;
}

.rounded {
  border-radius: 0.25rem;
}

.rounded-top {
  border-top-left-radius: 0.25rem;
  border-top-right-radius: 0.25rem;
}

.rounded-right {
  border-top-right-radius: 0.25rem;
  border-bottom-right-radius: 0.25rem;
}

.rounded-bottom {
  border-bottom-right-radius: 0.25rem;
  border-bottom-left-radius: 0.25rem;
}

.rounded-left {
  border-top-left-radius: 0.25rem;
  border-bottom-left-radius: 0.25rem;
}

.shadow {
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.125);
}

.shadow-sm {
  box-shadow: 0 0.125rem 0.25rem rgba(0, 0, 0, 0.075);
}

.shadow-md {
  box-shadow: 0 0.5rem 1rem rgba(0, 0, 0, 0.15);
}

.shadow-lg {
  box-shadow: 0 1rem 2rem rgba(0, 0, 0, 0.2);
}

.shadow-xl {
  box-shadow: 0 2rem 4rem rgba(0, 0, 0, 0.25);
}

/* Media queries */

@media (min-width: 768px) {
  .col-md-2 {
    flex: 2;
  }

  .col-md-3 {
    flex: 3;
  }

  .col-md-4 {
    flex: 4;
  }
}

@media (min-width: 992px) {
  .col-lg-2 {
    flex: 2;
  }

  .col-lg-3 {
    flex: 3;
  }

  .col-lg-4 {
    flex: 4;
  }
}

@media (min-width: 1200px) {
  .col-xl-2 {
    flex: 2;
  }

  .col-xl-3 {
    flex: 3;
  }

  .col-xl-4 {
    flex: 4;
  }
}
```

This code is a complete CSS stylesheet that can be used to style a web page. It includes styles for typography, layout, header, navigation, main content, footer, and utilities. It also includes media queries for different screen sizes.

The code is well-organized and commented, making it easy to read and understand. It uses a consistent naming convention, making it easy to find the styles you need.

The code is also very comprehensive, covering a wide range of styles. This makes it a great resource for web developers who want to create a professional-looking website.