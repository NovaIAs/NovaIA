```css
/*- Reset Styles -*/
*{margin: 0; padding: 0; box-sizing: border-box;}

/*Global Styles*/
html, body{height: 100%;font-family: sans-serif;}

/*Components*/
.container{
  width: 100%;
  max-width: 1200px;
  margin: 0 auto;
  padding: 1rem;
}

.header{
  background-color: #333;
  color: #fff;
  padding: 1rem;
}

.header h1{
  font-size: 2rem;
  margin-bottom: 0.5rem;
}

.header nav{
  float: right;
}

.header nav ul{
  list-style-type: none;
  display: flex;
}

.header nav li{
  margin-right: 1rem;
}

.header nav a{
  text-decoration: none;
  color: #fff;
}

.main{
  margin-top: 1rem;
}

.main article{
  background-color: #fff;
  padding: 1rem;
  margin-bottom: 1rem;
}

.main article h2{
  font-size: 1.5rem;
  margin-bottom: 0.5rem;
}

.main article p{
  font-size: 1rem;
  line-height: 1.5rem;
}

.sidebar{
  float: right;
  width: 20%;
  margin-left: 1rem;
}

.sidebar widget{
  background-color: #f5f5f5;
  padding: 1rem;
  margin-bottom: 1rem;
}

.sidebar widget h3{
  font-size: 1.5rem;
  margin-bottom: 0.5rem;
}

.sidebar widget ul{
  list-style-type: none;
}

.sidebar widget li{
  margin-bottom: 0.5rem;
}

.sidebar widget a{
  text-decoration: none;
}

.footer{
  background-color: #333;
  color: #fff;
  padding: 1rem;
  text-align: center;
}

/*Media Queries*/
@media screen and (max-width: 768px){
  .sidebar{
    float: none;
    width: 100%;
    margin-top: 1rem;
  }
}
```

The explanation of the code is as follows:

**Global Styles:** The global styles are the styles that are applied to all elements in the document. In this case, we are setting the margin and padding to 0 for all elements, using the box-sizing border-box, and setting the font-family to sans-serif.

**Components:** The component styles are the styles that are applied to specific elements in the document. In this case, we are styling the header, main, sidebar, and footer elements.

**Header:** The header style includes a background color, a color, and padding. It also includes a heading and a navigation bar. The navigation bar is a list of links, and each link is styled with a margin-right of 1rem and a text-decoration of none.

**Main:** The main style includes a margin-top of 1rem. It also includes styles for the articles, which are the main content of the page. Each article has a background color, padding, and margin-bottom. The article heading is styled with a font-size of 1.5rem and a margin-bottom of 0.5rem. The article paragraphs are styled with a font-size of 1rem and a line-height of 1.5rem.

**Sidebar:** The sidebar style includes a float to the right, a width of 20%, and a margin-left of 1rem. It also includes styles for the widgets, which are the content blocks in the sidebar. Each widget has a background color, padding, and margin-bottom. The widget heading is styled with a font-size of 1.5rem and a margin-bottom of 0.5rem. The widget list is a list of links, and each link is styled with a margin-bottom of 0.5rem and a text-decoration of none.

**Footer:** The footer style includes a background color, a color, padding, and text-align center.

**Media Queries:** The media query is used to apply styles to elements based on the width of the screen. In this case, we are applying styles to the sidebar when the screen width is less than or equal to 768px. When the screen width is less than or equal to 768px, we are setting the sidebar to float none, setting the width to 100%, and setting the margin-top to 1rem.