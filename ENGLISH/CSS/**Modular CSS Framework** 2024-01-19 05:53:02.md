```css
/* Variables */

:root {
  --primary-color: #123456;
  --secondary-color: #7890ab;
  --font-family: sans-serif;
  --spacing: 1rem;
  --border-radius: 5px;
}

/* Typography */

body {
  font-family: var(--font-family);
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
  margin-bottom: var(--spacing);
}

p {
  margin-bottom: var(--spacing);
}

/* Links */

a {
  color: var(--primary-color);
  text-decoration: none;
}

a:hover {
  color: var(--secondary-color);
}

/* Buttons */

button {
  padding: var(--spacing);
  border: 1px solid var(--primary-color);
  border-radius: var(--border-radius);
  background-color: var(--primary-color);
  color: white;
  font-weight: bold;
  cursor: pointer;
}

button:hover {
  background-color: var(--secondary-color);
}

/* Forms */

input,
textarea {
  padding: var(--spacing);
  border: 1px solid var(--primary-color);
  border-radius: var(--border-radius);
  font-size: 16px;
}

input:focus,
textarea:focus {
  outline: none;
  border-color: var(--secondary-color);
}

/* Layouts */

.container {
  max-width: 960px;
  padding: var(--spacing);
  margin: 0 auto;
}

.row {
  display: flex;
  flex-wrap: wrap;
}

.col {
  flex: 1 1 auto;
  padding: var(--spacing);
}

.col-2 {
  flex: 0 0 50%;
}

.col-3 {
  flex: 0 0 33.33%;
}

.col-4 {
  flex: 0 0 25%;
}

/* Media Queries */

@media (max-width: 768px) {
  .col-2,
  .col-3,
  .col-4 {
    flex-basis: 100%;
  }
}
```

This code is a complex and differentiated CSS code that includes variables, typography, links, buttons, forms, layouts, and media queries. The code is well-organized and uses meaningful variable names to make it easy to understand and maintain. Here is a breakdown of the code:

**Variables:**

The `:root` rule is used to define global variables that can be used throughout the code. In this case, the variables include the primary and secondary colors, the font family, the spacing, and the border radius.

**Typography:**

The code includes styles for headings, paragraphs, and links. The headings are bold and have a bottom margin of 1rem. The paragraphs have a bottom margin of 1rem as well. The links are blue by default and turn purple when hovered over.

**Buttons:**

The code includes styles for buttons. The buttons have a padding of 1rem, a border of 1px solid blue, a border radius of 5px, a background color of blue, a white color, a bold font weight, and a cursor pointer. The buttons turn purple when hovered over.

**Forms:**

The code includes styles for input fields and textareas. The input fields and textareas have a padding of 1rem, a border of 1px solid blue, a border radius of 5px, a font size of 16px, and an outline of none. The input fields and textareas turn purple when focused.

**Layouts:**

The code includes styles for containers, rows, and columns. The container is a div with a maximum width of 960px, padding of 1rem, and a margin of 0 auto. The row is a div with a flexbox display and flex-wrap set to wrap. The columns are divs with a flex basis of 1 1 auto, padding of 1rem, and a margin of 0 auto. The columns also have different flex sizes for different screen sizes, as defined by the media queries.

**Media Queries:**

The code includes media queries for screens with a maximum width of 768px. The media queries are used to make the columns stack on top of each other on smaller screens.

This code is a good example of how to use CSS to create a complex and differentiated layout with a variety of elements. The code is well-organized and uses meaningful variable names to make it easy to understand and maintain.