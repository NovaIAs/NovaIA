```css
/* CSS Selectors */

/* 1. Universal Selector */
* {
  font-family: 'Helvetica', 'Arial', sans-serif;
  margin: 0;
  padding: 0;
}

/* 2. Class Selector */
.container {
  width: 100%;
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
}

/* 3. ID Selector */
#header {
  background-color: #f1f1f1;
  padding: 20px;
  text-align: center;
}

/* 4. Element Selector */
p {
  font-size: 16px;
  line-height: 1.6;
}

/* 5. Descendant Selector */
ul li a {
  text-decoration: none;
  color: #000;
}

/* 6. Child Selector */
ul > li {
  display: inline-block;
  margin-right: 20px;
}

/* 7. Adjacent Sibling Selector */
h2 + p {
  margin-top: 10px;
}

/* 8. General Sibling Selector */
h2 ~ p {
  margin-top: 10px;
}

/* 9. Attribute Selector */
a[href*="google.com"] {
  color: #000;
}

/* 10. Pseudo-Class Selectors */
:hover {
  color: #f00;
}

:active {
  color: #0f0;
}

:focus {
  outline: 2px solid #00f;
}

:visited {
  color: #888;
}

/* CSS Properties */

/* 1. Font Properties */
font-family: 'Helvetica', 'Arial', sans-serif;
font-size: 16px;
line-height: 1.6;
font-weight: normal;
font-style: normal;
text-align: center;
text-decoration: none;
color: #000;

/* 2. Box Model Properties */
width: 100%;
max-width: 1200px;
margin: 0 auto;
padding: 20px;

/* 3. Background Properties */
background-color: #f1f1f1;

/* 4. Border Properties */
border: 1px solid #000;
border-radius: 5px;

/* 5. Display Properties */
display: inline-block;

/* 6. Flexbox Properties */
flex-direction: row;
flex-wrap: wrap;
justify-content: center;
align-items: center;
align-content: center;

/* 7. Grid Properties */
grid-template-columns: repeat(3, 1fr);
grid-gap: 20px;

/* 8. Animation Properties */
animation-name: fadein;
animation-duration: 2s;
animation-timing-function: ease-in-out;
animation-iteration-count: infinite;

/* CSS Media Queries */

/* 1. Mobile Devices */
@media screen and (max-width: 768px) {
  .container {
    max-width: 100%;
  }
}

/* 2. Tablets */
@media screen and (min-width: 769px) and (max-width: 1024px) {
  .container {
    max-width: 768px;
  }
}

/* 3. Desktops */
@media screen and (min-width: 1025px) {
  .container {
    max-width: 1200px;
  }
}

/* 4. Print Media */
@media print {
  * {
    color: #000 !important;
    background-color: #fff !important;
  }
}
```

**Explanation:**

This CSS code includes various selectors, properties, and media queries to style a web page. It incorporates universal, class, ID, element, descendant, child, adjacent sibling, general sibling, attribute, and pseudo-class selectors. It covers various properties, including font styles, box model, background, border, display, flexbox, grid, and animation. Moreover, it contains media queries for different devices, including mobile, tablet, and desktop, as well as print media. The code demonstrates the power and flexibility of CSS in creating complex and responsive designs.