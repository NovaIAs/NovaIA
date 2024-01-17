```css
/* CSS Reset */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Typography */

html {
  font-size: 62.5%; /* 1rem = 10px */
}

body {
  font-family: 'Helvetica', 'Arial', sans-serif;
  font-size: 1.6rem; /* 16px */
  line-height: 1.5;
  color: #333;
}

h1, h2, h3, h4, h5, h6 {
  font-weight: bold;
  margin-bottom: 1.2rem; /* 19.2px */
}

h1 {
  font-size: 3.6rem; /* 57.6px */
}

h2 {
  font-size: 2.8rem; /* 44.8px */
}

h3 {
  font-size: 2.2rem; /* 35.2px */
}

h4 {
  font-size: 1.8rem; /* 28.8px */
}

h5 {
  font-size: 1.6rem; /* 25.6px */
}

h6 {
  font-size: 1.4rem; /* 22.4px */
}

p {
  margin-bottom: 1.2rem; /* 19.2px */
}

/* Layout */

.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 1.2rem; /* 19.2px */
}

.row {
  display: flex;
  flex-wrap: wrap;
  margin-right: -1.2rem; /* -19.2px */
  margin-left: -1.2rem; /* -19.2px */
}

.column {
  padding-right: 1.2rem; /* 19.2px */
  padding-left: 1.2rem; /* 19.2px */
}

/* Colors */

.primary-color {
  color: #007bff;
}

.secondary-color {
  color: #6c757d;
}

/* Buttons */

.btn {
  display: inline-block;
  padding: 0.6rem 1.2rem; /* 9.6px 19.2px */
  border: 1px solid #007bff;
  border-radius: 0.2rem; /* 3.2px */
  text-decoration: none;
  color: #007bff;
  font-weight: bold;
  text-align: center;
  vertical-align: middle;
  cursor: pointer;
}

.btn:hover {
  background-color: #007bff;
  color: #fff;
}

/* Forms */

.form-control {
  width: 100%;
  padding: 0.6rem 1.2rem; /* 9.6px 19.2px */
  border: 1px solid #ced4da;
  border-radius: 0.2rem; /* 3.2px */
}

.form-control:focus {
  border-color: #007bff;
}

/* Other styles */

.visually-hidden {
  display: none;
}
```

**Explanation:**

* **CSS Reset:** This section resets the default styles of all elements to a consistent starting point.
* **Typography:** This section defines the fonts, font sizes, and line heights for the different elements.
* **Layout:** This section defines the layout of the page, including the container, rows, and columns.
* **Colors:** This section defines the colors used throughout the page.
* **Buttons:** This section defines the styles for buttons.
* **Forms:** This section defines the styles for form elements.
* **Other styles:** This section includes other miscellaneous styles, such as the styles for visually hidden elements.

This code creates a basic HTML5 template with a clean and modern design. It includes styles for typography, layout, colors, buttons, forms, and other elements.