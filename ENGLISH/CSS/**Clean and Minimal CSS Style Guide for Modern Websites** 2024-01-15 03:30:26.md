```css
/* Reset Styles */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Typography */

body {
  font-family: 'Helvetica', 'Arial', sans-serif;
  font-size: 16px;
  line-height: 1.5;
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

p {
  font-size: 1rem;
  line-height: 1.5;
}

/* Layout */

.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
}

.header {
  background-color: #f1f1f1;
  padding: 20px;
}

.main {
  margin-top: 20px;
}

.sidebar {
  float: right;
  width: 300px;
  margin-left: 20px;
}

.content {
  width: calc(100% - 320px);
}

/* Color Palette */

:root {
  --primary-color: #007bff;
  --secondary-color: #6c757d;
  --tertiary-color: #ffc107;
}

/* Link Styles */

a {
  color: var(--primary-color);
  text-decoration: none;
}

a:hover {
  color: var(--secondary-color);
}

/* Button Styles */

button {
  background-color: var(--primary-color);
  color: white;
  padding: 10px 20px;
  border: none;
  border-radius: 5px;
}

button:hover {
  background-color: var(--secondary-color);
}

/* Form Styles */

input {
  width: 100%;
  padding: 10px;
  border: 1px solid #ccc;
  border-radius: 5px;
}

textarea {
  width: 100%;
  height: 200px;
  padding: 10px;
  border: 1px solid #ccc;
  border-radius: 5px;
}

/* Media Queries */

@media (max-width: 992px) {
  .sidebar {
    float: none;
    width: 100%;
    margin-top: 20px;
  }

  .content {
    width: 100%;
  }
}
```

Explanation:

* The code starts with a reset styles section, which sets all elements to have a margin and padding of 0, and a box-sizing of border-box. This ensures that all elements are the same size as their content, without any extra space.
* The typography section sets the font family, font size, and line height for different elements, such as headings and paragraphs.
* The layout section defines the structure of the page, with a container, header, main, and sidebar. The container is the main content area, the header contains the site's logo and navigation, the main contains the main content of the page, and the sidebar contains additional information or links.
* The color palette section defines the primary, secondary, and tertiary colors used throughout the site.
* The link styles section defines the styles for links, including the color and hover color.
* The button styles section defines the styles for buttons, including the background color, color, padding, border, and border radius.
* The form styles section defines the styles for form elements, such as inputs and textareas.
* The media queries section defines styles that are applied only when the screen size is below a certain width. In this case, the sidebar is moved below the main content when the screen size is below 992 pixels.