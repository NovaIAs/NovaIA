```css
/* CSS Reset (normalize.css) */
*, *:before, *:after {
  box-sizing: border-box;
  margin: 0;
  padding: 0;
}

body {
  font-family: 'Helvetica', 'Arial', sans-serif;
  font-size: 16px;
  line-height: 1.5;
}

/* Layout */
.container {
  max-width: 1200px;
  padding: 0 15px;
  margin: 0 auto;
}

.header {
  background-color: #333;
  color: #fff;
  padding: 10px 0;
}

.header h1 {
  font-size: 24px;
  font-weight: bold;
}

.main {
  padding: 15px 0;
}

.sidebar {
  float: right;
  width: 25%;
  background-color: #f5f5f5;
  padding: 15px;
}

.content {
  float: left;
  width: 75%;
}

/* Typography */
h1, h2, h3, h4, h5, h6 {
  font-family: 'Georgia', serif;
  font-weight: bold;
}

h1 {
  font-size: 36px;
}

h2 {
  font-size: 24px;
}

h3 {
  font-size: 18px;
}

h4 {
  font-size: 16px;
}

h5 {
  font-size: 14px;
}

h6 {
  font-size: 12px;
}

p {
  font-size: 16px;
  line-height: 1.5;
}

/* Links */
a {
  color: #007bff;
  text-decoration: none;
}

a:hover {
  text-decoration: underline;
}

/* Buttons */
button {
  background-color: #007bff;
  color: #fff;
  padding: 10px 15px;
  font-size: 16px;
  font-weight: bold;
  border: none;
  cursor: pointer;
}

button:hover {
  background-color: #0069d9;
}

/* Forms */
input, textarea {
  width: 100%;
  padding: 10px;
  border: 1px solid #ccc;
  font-size: 16px;
}

input:focus, textarea:focus {
  border-color: #007bff;
}

/* Tables */
table {
  border-collapse: collapse;
  width: 100%;
}

th, td {
  padding: 10px;
  border: 1px solid #ccc;
}

th {
  text-align: left;
  font-weight: bold;
}

/* Media Queries */
@media screen and (max-width: 768px) {
  .sidebar {
    width: 100%;
    float: none;
  }

  .content {
    float: none;
    width: 100%;
  }
}

/* Custom Styles */
.hero {
  background-image: url('hero.jpg');
  background-size: cover;
  background-position: center;
  height: 500px;
}

.callout {
  background-color: #f5f5f5;
  padding: 20px;
  margin: 20px 0;
}

.success {
  color: #4caf50;
}

.error {
  color: #f44336;
}

/* Utility Classes */
.text-center {
  text-align: center;
}

.text-right {
  text-align: right;
}

.float-left {
  float: left;
}

.float-right {
  float: right;
}

.clear {
  clear: both;
}
```

This code is a complete and complex CSS style sheet that covers a wide range of elements and styles, including layout, typography, links, buttons, forms, tables, media queries, and custom styles. The code is well-organized and commented, making it easy to understand and modify.

Here is a brief explanation of some of the key parts of the code:

* The `/* CSS Reset (normalize.css) */` section resets all browser's default styles, ensuring that all elements start with the same baseline styles.
* The `/* Layout */` section defines the layout of the page, including the header, sidebar, and content areas.
* The `/* Typography */` section defines the styles for headings and paragraphs.
* The `/* Links */` section defines the styles for links.
* The `/* Buttons */` section defines the styles for buttons.
* The `/* Forms */` section defines the styles for form elements, such as inputs and textareas.
* The `/* Tables */` section defines the styles for tables.
* The `/* Media Queries */` section defines the styles for different screen sizes.
* The `/* Custom Styles */` section defines some custom styles for specific elements.
* The `/* Utility Classes */` section defines some utility classes that can be used to quickly style elements.

Overall, this code is a comprehensive and powerful CSS style sheet that can be used to create beautiful and functional websites.