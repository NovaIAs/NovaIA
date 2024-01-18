```css
/* CSS Code Starts Here */

/* Define a Global Font Family */
@font-face {
  font-family: 'Custom Font';
  src: url('./fonts/custom-font.otf');
}

/* Reset CSS Styles */
* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

/* Overall Structure Styles */
body {
  font-family: 'Custom Font', Arial, Helvetica, sans-serif;
  line-height: 1.6;
  color: #333;
  background-color: #fff;
}

/* Header Styles */
header {
  background-color: #f5f5f5;
  padding: 20px 0;
}

header h1 {
  font-size: 24px;
  font-weight: bold;
  color: #444;
  text-align: center;
}

header nav {
  text-align: center;
}

header nav ul {
  list-style-type: none;
  display: flex;
  justify-content: center;
  align-items: center;
}

header nav li {
  margin-right: 20px;
}

header nav a {
  text-decoration: none;
  color: #444;
  font-size: 16px;
}

header nav a:hover {
  color: #000;
}

/* Main Content Styles */
main {
  padding: 20px;
}

main h2 {
  font-size: 20px;
  font-weight: bold;
  color: #444;
}

main p {
  font-size: 16px;
  line-height: 1.6;
}

/* Footer Styles */
footer {
  background-color: #f5f5f5;
  padding: 20px 0;
  text-align: center;
}

footer p {
  font-size: 14px;
  color: #666;
}

/* Responsive Styles */
@media (max-width: 768px) {
  header nav ul {
    display: block;
  }

  header nav li {
    margin: 10px 0;
  }
}

/* CSS Code Ends Here */
```

This code provides a comprehensive style guide for a web page, covering various aspects of design and layout. It includes typography, colors, headers, navigation, main content, footer, and responsive styles. Here's an explanation of the code:

1. **Font Family:**
   - The code starts by defining a custom font family. It loads a font file from the `./fonts/custom-font.otf` path.

2. **Reset CSS Styles:**
   - The `*` selector resets all default browser styles for elements like margins, padding, and box-sizing.

3. **Overall Structure:**
   - The body styles define a global font family, line height, color, and background color.

4. **Header:**
   - The header has a background color of `#f5f5f5`, padding, and a centered `<h1>` element.
   - Inside the header, there's a navigation section with a centered `<ul>` containing links.

5. **Main Content:**
   - The main content is styled with padding.
   - There are `<h2>` elements for section headings and `<p>` elements for paragraph text.

6. **Footer:**
   - The footer has a background color of `#f5f5f5`, padding, and centered text.
   - Inside the footer, there's a `<p>` element for copyright or attribution information.

7. **Responsive Styles:**
   - The code includes media queries for responsive design. When the screen width is 768px or less, the navigation elements are displayed vertically instead of horizontally.

8. **Detailed Styling:**
   - The code also includes detailed styling for various elements such as links, hover states, and typography.

Overall, this CSS code demonstrates advanced styling techniques and provides a solid foundation for building a visually appealing and responsive web page. It serves as a comprehensive example of CSS usage, covering various aspects of web design.