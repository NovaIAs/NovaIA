```css
/* 1. Universal Selectors */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* 2. HTML Elements */

body {
  font-family: Arial, Helvetica, sans-serif;
  background-color: #efefef;
}

h1 {
  font-size: 2.5rem;
  font-weight: bold;
  color: #333;
}

h2 {
  font-size: 1.5rem;
  font-weight: bold;
  color: #666;
}

p {
  font-size: 1rem;
  line-height: 1.5;
  color: #333;
}

a {
  color: #007bff;
  text-decoration: none;
}

a:hover {
  color: #0056b3;
  text-decoration: underline;
}

/* 3. Layout */

.container {
  max-width: 1024px;
  margin: 0 auto;
  padding: 2rem;
}

.row {
  display: flex;
  flex-direction: row;
  flex-wrap: wrap;
}

.column {
  flex-basis: 100%;
  flex-grow: 1;
}

.column-half {
  flex-basis: 50%;
  flex-grow: 1;
}

/* 4. Forms */

form {
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

label {
  font-weight: bold;
}

input,
textarea {
  width: 100%;
  padding: 0.5rem;
  border: 1px solid #ccc;
  border-radius: 4px;
}

input:focus,
textarea:focus {
  outline: none;
  border-color: #007bff;
}

/* 5. Buttons */

button {
  padding: 0.5rem 1rem;
  border: 1px solid #ccc;
  border-radius: 4px;
  background-color: #007bff;
  color: #fff;
  cursor: pointer;
}

button:hover {
  background-color: #0056b3;
}

/* 6. Media Queries */

@media (max-width: 768px) {
  .column {
    flex-basis: 100%;
  }

  .column-half {
    flex-basis: 100%;
  }
}

/* 7. Custom Classes */

.success {
  color: #28a745;
}

.error {
  color: #dc3545;
}

.hidden {
  display: none;
}
```

Explanation:

1. Universal Selectors:

   - The universal selector `*` selects all elements in the document.
   - It sets common CSS properties, such as margin, padding, and box-sizing, for all elements.

2. HTML Elements:

   - This section defines styles for specific HTML elements, like headings, paragraphs, links, etc.
   - For example, `h1` sets the font size to 2.5rem, bold font weight, and black color.

3. Layout:

   - The `.container` class defines a container that centers the content inside it.
   - The `.row` class defines a flexbox container that wraps its flex items (columns) on multiple lines.
   - The `.column` and `.column-half` classes define flex items that grow and shrink to fill the available space.

4. Forms:

   - Styles for form elements like labels, inputs, and textarea are defined here.
   - Inputs and textareas are given a border, padding, and rounded corners.

5. Buttons:

   - The `.button` class styles buttons with a blue background, white text, and a rounded border.
   - The button has a hover effect that changes the background color to a darker blue.

6. Media Queries:

   - The media query targets screens with a maximum width of 768px.
   - In this scenario, it makes all columns full-width to optimize the layout for smaller screens.

7. Custom Classes:

   - The `.success`, `.error`, and `.hidden` classes can be applied to elements to add specific styles.
   - For example, the `.success` class can be used to highlight successful actions, such as a form submission.