```css
/* Container styles */
.container {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  text-align: center;
  height: 100vh;
  background-color: #f2f2f2;
}

/* Header styles */
.header {
  font-family: Arial, sans-serif;
  font-size: 3rem;
  font-weight: bold;
  color: #000;
}

/* Main content styles */
.main {
  font-family: Arial, sans-serif;
  font-size: 1.5rem;
  color: #333;
  line-height: 1.5;
}

/* Button styles */
.button {
  display: inline-block;
  padding: 10px 20px;
  border: 1px solid #000;
  border-radius: 5px;
  background-color: #000;
  color: #fff;
  text-decoration: none;
  font-family: Arial, sans-serif;
  font-size: 1rem;
  font-weight: bold;
}

/* Hover state for button */
.button:hover {
  background-color: #333;
}

/* Media query for small screens */
@media (max-width: 768px) {
  .container {
    flex-direction: column;
    align-items: center;
    justify-content: center;
  }

  .header {
    font-size: 2rem;
  }

  .main {
    font-size: 1rem;
  }
}
```

Explanation:

This CSS code creates a simple web page with a header, main content, and a button. It uses media queries to adjust the layout for small screens.

The container is a flex container that displays its children in a column. The flex-direction property sets the direction of the flex items. In this case, the flex items are displayed vertically.

The header is a div with the class name "header". It contains the page title. The font-family, font-size, font-weight, and color properties are used to style the header text.

The main div contains the main content of the page. It contains a paragraph of text and a button. The font-family, font-size, color, and line-height properties are used to style the main content text.

The button is an anchor tag with the class name "button". It has a display property of inline-block, which allows it to be placed inline with other elements. The padding, border, border-radius, background-color, color, text-decoration, font-family, font-size, and font-weight properties are used to style the button.

The hover state for the button is defined using the :hover pseudo-class. When the user hovers over the button, its background color changes to #333.

The media query at the bottom of the code is used to adjust the layout for small screens. When the screen width is less than or equal to 768 pixels, the container flexes its children in a column instead of a row. The header and main text are also made smaller.

This CSS code is a good example of how to use flexbox, media queries, and CSS properties to create a responsive web page.