```css
/* CSS CODE */

/* Base styles */
body {
  font-family: sans-serif;
  background-color: #f5f5f5;
}

h1, h2, h3, h4, h5, h6 {
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

/* Layout */

.container {
  max-width: 960px;
  margin: 0 auto;
  padding: 0 15px;
}

.row {
  display: flex;
  flex-wrap: wrap;
  margin-right: -15px;
  margin-left: -15px;
}

.col {
  padding: 0 15px;
}

.col-12 {
  flex-basis: 100%;
}

.col-6 {
  flex-basis: 50%;
}

.col-4 {
  flex-basis: 33.3333%;
}

.col-3 {
  flex-basis: 25%;
}

.col-2 {
  flex-basis: 16.6666%;
}

/* Typography */

h1 {
  font-size: 2.5rem;
}

h2 {
  font-size: 2rem;
}

h3 {
  font-size: 1.75rem;
}

h4 {
  font-size: 1.5rem;
}

h5 {
  font-size: 1.25rem;
}

h6 {
  font-size: 1rem;
}

/* Buttons */

.btn {
  display: inline-block;
  padding: 6px 12px;
  border: 1px solid #ccc;
  border-radius: 4px;
  background-color: #fff;
  color: #333;
  text-align: center;
  vertical-align: middle;
  font-size: 14px;
  line-height: 1.42857143;
  cursor: pointer;
}

.btn:hover {
  background-color: #e6e6e6;
}

.btn-primary {
  color: #fff;
  background-color: #007bff;
  border-color: #007bff;
}

.btn-primary:hover {
  background-color: #0069d9;
}

.btn-secondary {
  color: #fff;
  background-color: #6c757d;
  border-color: #6c757d;
}

.btn-secondary:hover {
  background-color: #56616d;
}

/* Forms */

.form-control {
  display: block;
  width: 100%;
  padding: 6px 12px;
  border: 1px solid #ccc;
  border-radius: 4px;
  background-color: #fff;
  color: #333;
  font-size: 14px;
  line-height: 1.42857143;
}

.form-control:focus {
  border-color: #007bff;
}

.form-group {
  margin-bottom: 15px;
}

/* Tables */

table {
  border-collapse: collapse;
  border-spacing: 0;
  width: 100%;
}

th, td {
  padding: 8px;
  border: 1px solid #ccc;
}

thead {
  background-color: #f5f5f5;
}

tbody {
  background-color: #ffffff;
}

/* Media queries */

@media (min-width: 768px) {
  .col-lg-12 {
    flex-basis: 100%;
  }

  .col-lg-6 {
    flex-basis: 50%;
  }

  .col-lg-4 {
    flex-basis: 33.3333%;
  }

  .col-lg-3 {
    flex-basis: 25%;
  }

  .col-lg-2 {
    flex-basis: 16.6666%;
  }
}
```

**Explanation:**

This CSS code is a comprehensive and versatile stylesheet that covers a wide range of HTML elements and use cases. It includes styles for base elements, layout, typography, buttons, forms, tables, and media queries.

* **Base styles:** These styles define the default font family, background color, and styles for headings, paragraphs, and links.
* **Layout:** The code includes styles for creating flexible layouts using flexbox. It defines classes for creating rows and columns of different sizes.
* **Typography:** This section includes styles for different heading levels and paragraph text.
* **Buttons:** The code defines styles for different types of buttons, including primary and secondary buttons.
* **Forms:** The code includes styles for form controls such as input fields, textareas, and buttons.
* **Tables:** The code includes styles for tables, including headers and rows.
* **Media queries:** The code includes media queries to adjust the layout and styles for different screen sizes.

Overall, this code provides a solid foundation for creating visually appealing and responsive websites. It is well-organized and easy to understand, making it a good starting point for web designers and developers.