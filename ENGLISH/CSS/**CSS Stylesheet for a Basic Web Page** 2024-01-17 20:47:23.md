```css
/* Global Styles */

body {
  font-family: Arial, sans-serif;
  font-size: 16px;
  line-height: 1.5;
  color: #333;
  background-color: #fff;
}

h1, h2, h3, h4, h5, h6 {
  font-weight: bold;
}

p {
  margin-bottom: 1em;
}

a {
  color: #007bff;
  text-decoration: none;
}

a:hover {
  color: #0056b3;
}

/* Layout Styles */

.container {
  max-width: 960px;
  padding: 0 15px;
}

.row {
  display: flex;
  flex-wrap: wrap;
}

.col {
  flex: 1;
  padding: 15px;
}

/* Component Styles */

.card {
  background-color: #fff;
  border: 1px solid #eee;
  border-radius: 5px;
  padding: 20px;
}

.card-header {
  background-color: #f8f9fa;
  border-bottom: 1px solid #eee;
  padding: 15px;
}

.card-body {
  padding: 20px;
}

.card-footer {
  background-color: #f8f9fa;
  border-top: 1px solid #eee;
  padding: 15px;
}

.btn {
  display: inline-block;
  padding: 6px 12px;
  border: 1px solid #ccc;
  border-radius: 5px;
  text-decoration: none;
}

.btn-primary {
  background-color: #007bff;
  color: #fff;
  border-color: #007bff;
}

.btn-primary:hover {
  background-color: #0069d9;
  border-color: #0069d9;
}

/* Media Queries */

@media (max-width: 768px) {
  .col {
    flex-basis: 100%;
  }
}
```

This CSS code defines a set of styles for a web page. It includes global styles, layout styles, component styles, and media queries.

**Global Styles:**

The `body` selector selects the entire body of the web page. It sets the `font-family`, `font-size`, `line-height`, `color`, and `background-color`.

The `h1`, `h2`, `h3`, `h4`, `h5`, and `h6` selectors select all the heading elements on the page. They set the `font-weight` to bold.

The `p` selector selects all the paragraph elements on the page. It sets the `margin-bottom` to 1em.

The `a` selector selects all the anchor elements on the page. It sets the `color` and `text-decoration`. The `a:hover` selector selects all the anchor elements when the user hovers over them. It changes the `color`.

**Layout Styles:**

The `container` selector selects the container element on the page. It sets the `max-width` and `padding`.

The `row` selector selects all the row elements on the page. It sets the `display` to `flex` and `flex-wrap` to `wrap`.

The `col` selector selects all the column elements on the page. It sets the `flex`, `padding`, and `margin`.

**Component Styles:**

The `.card` selector selects all the card elements on the page. It sets the `background-color`, `border`, `border-radius`, and `padding`.

The `.card-header` selector selects all the card header elements on the page. It sets the `background-color`, `border-bottom`, and `padding`.

The `card-body` selector selects all the card body elements on the page. It sets the `padding`.

The `.card-footer` selector selects all the card footer elements on the page. It sets the `background-color`, `border-top`, and `padding`.

The `.btn` selector selects all the button elements on the page. It sets the `display`, `padding`, `border`, `border-radius`, and `text-decoration`.

The `.btn-primary` selector selects all the primary button elements on the page. It sets the `background-color`, `color`, and `border-color`. The `.btn-primary:hover` selector selects all the primary button elements when the user hovers over them. It changes the `background-color` and `border-color`.

**Media Queries:**

The `@media (max-width: 768px)` media query selects all the elements on the page when the screen width is 768px or less. It changes the `flex-basis` of the `.col` elements to 100%.