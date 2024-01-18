```css
/* OVERRIDING USER AGENT STYLESHEET */

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
  font-family: sans-serif;
  color: #333;
}

/* ADDING BASE STYLES */

body {
  background-color: #fafafa;
}

h1 {
  font-size: 36px;
  font-weight: bold;
  text-align: center;
}

p {
  font-size: 16px;
  line-height: 1.5em;
  text-align: justify;
}

a {
  color: #000;
  text-decoration: none;
}

a:hover {
  color: #f00;
  text-decoration: underline;
}

/* CREATING A LAYOUT WITH FLEXBOX */

.container {
  display: flex;
  flex-direction: row;
  justify-content: center;
  align-items: center;
  height: 100vh;
}

.column {
  flex: 1;
  padding: 20px;
}

/* ADDING SOME VISUAL APPEAL */

.column:nth-child(2) {
  background-color: #f0f0f0;
}

.column:nth-child(3) {
  background-color: #e0e0e0;
}

/* USING CSS GRID FOR THE FOOTER */

footer {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  background-color: #333;
  color: #fff;
  padding: 20px;
}

footer div {
  text-align: center;
}

/* MEDIA QUERIES FOR RESPONSIVE DESIGN */

@media (max-width: 768px) {
  .container {
    flex-direction: column;
  }
}

@media (max-width: 480px) {
  footer {
    grid-template-columns: 1fr;
  }
}
```

Explanation:

- The code starts by overriding the user agent stylesheet, setting common styles for all elements.
- Base styles are added, including font family, size, color, and margin/padding reset.
- A container is created using Flexbox, which is a layout system that allows for flexible spacing of elements.
- Within the container, there are three columns, each with different background colors, creating a visual separation.
- A footer is created using CSS Grid, which is a newer layout system that allows for more complex layouts.
- The footer is divided into three columns, each with centered text.
- Media queries are used to ensure the layout is responsive and adapts to different screen sizes.